package io.sphere.json.generic

import cats.data.Validated
import cats.syntax.traverse.*
import io.sphere.json.field
import io.sphere.json.generic.{AnnotationReader, CaseClassMetaData, Field, TraitMetaData}
import org.json4s.JsonAST.*
import org.json4s.DefaultReaders.StringReader
import org.json4s.{jvalue2monadic, jvalue2readerSyntax}

import scala.deriving.Mirror

import io.sphere.json.FromJSON
import io.sphere.json.*

trait DeriveFromJSON {
  inline given derived[A](using Mirror.Of[A]): FromJSON[A] = Derivation.derived[A]

  protected object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): FromJSON[A] =
      inline m match {
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): FromJSON[A] = {
      val traitMetaData: TraitMetaData = AnnotationReader.readTraitMetaData[A]

      val typeHintMap: Map[String, String] = traitMetaData.subtypes.flatMap {
        case (name, classMeta) if classMeta.typeHint.isDefined =>
          classMeta.typeHint.map(name -> _)
        case _ =>
          None
      }

      val reverseTypeHintMap: Map[String, String] = typeHintMap.map((on, n) => (n, on))
      val fromJsons: Seq[FromJSON[Any]] = summonFromJsons[mirrorOfSum.MirroredElemTypes]

      val names: Seq[String] =
        constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
          .asInstanceOf[Vector[String]]

      val jsonsByNames: Map[String, FromJSON[Any]] = names.zip(fromJsons).toMap

      FromJSON.instance(
        readFn = {
          case jObject: JObject =>
            val typeName = (jObject \ traitMetaData.typeDiscriminator).as[String]
            val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
            jsonsByNames(originalTypeName).read(jObject).map(_.asInstanceOf[A])

          case x =>
            Validated.invalidNel(JSONParseError(s"JSON object expected. Got: '$x'"))
        }
      )
    }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): FromJSON[A] = {
      val caseClassMetaData: CaseClassMetaData = AnnotationReader.readCaseClassMetaData[A]
      val fromJsons: Vector[FromJSON[Any]] =
        summonFromJsons[mirrorOfProduct.MirroredElemTypes]
      val fieldsAndJsons: Vector[(Field, FromJSON[Any])] =
        caseClassMetaData.fields.zip(fromJsons)

      val fieldNames: Vector[String] = fieldsAndJsons.flatMap { (field, fromJson) =>
        if (field.embedded) fromJson.fields.toVector :+ field.name
        else Vector(field.name)
      }

      def readField(field: Field, fromJson: FromJSON[Any], jObject: JObject): JValidation[Any] =
        if (field.embedded) fromJson.read(jObject)
        else io.sphere.json.field(field.fieldName, field.defaultArgument)(jObject)(fromJson)

      FromJSON.instance(
        readFn = {
          case jObject: JObject =>
            for {
              fieldsAsAList <-
                fieldsAndJsons.traverse((field, fromJson) => readField(field, fromJson, jObject))

              fieldsAsTuple = Tuple.fromArray(fieldsAsAList.toArray)
            } yield mirrorOfProduct.fromTuple(
              fieldsAsTuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

          case x =>
            Validated.invalidNel(JSONParseError(s"JSON object expected. $x"))
        },
        fieldSet = fieldNames.toSet
      )
    }

    inline private def summonFromJsons[T <: Tuple]: Vector[FromJSON[Any]] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[FromJSON[t]]
            .asInstanceOf[FromJSON[Any]] +: summonFromJsons[ts]
      }

  }

}
