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

trait DeriveJSON {
  inline given derived[A](using Mirror.Of[A]): FromJSON[A] = Derived.derived[A]

  object Derived {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): FromJSON[A] =
      inline m match {
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): FromJSON[A] =
      new FromJSON[A] {
        private val traitMetaData: TraitMetaData = AnnotationReader.readTraitMetaData[A]
        private val typeHintMap: Map[String, String] = traitMetaData.subtypes.collect {
          case (name, classMeta) if classMeta.typeHint.isDefined =>
            name -> classMeta.typeHint.get
        }
        private val reverseTypeHintMap: Map[String, String] = typeHintMap.map((on, n) => (n, on))
        private val fromJsons: Seq[FromJSON[Any]] = summonFromJsons[mirrorOfSum.MirroredElemTypes]
        private val names: Seq[String] =
          constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
            .asInstanceOf[Vector[String]]
        private val jsonsByNames: Map[String, FromJSON[Any]] = names.zip(fromJsons).toMap

        override def read(jValue: JValue): JValidation[A] =
          jValue match {
            case jObject: JObject =>
              val typeName = (jObject \ traitMetaData.typeDiscriminator).as[String]
              val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
              jsonsByNames(originalTypeName).read(jObject).map(_.asInstanceOf[A])
            case x =>
              Validated.invalidNel(JSONParseError(s"JSON object expected. Got: '$jValue'"))
          }
      }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): FromJSON[A] =
      new FromJSON[A] {
        private val caseClassMetaData: CaseClassMetaData = AnnotationReader.readCaseClassMetaData[A]
        private val fromJsons: Vector[FromJSON[Any]] =
          summonFromJsons[mirrorOfProduct.MirroredElemTypes]
        private val fieldsAndJsons: Vector[(Field, FromJSON[Any])] =
          caseClassMetaData.fields.zip(fromJsons)

        private val fieldNames: Vector[String] = fieldsAndJsons.flatMap { (field, fromJson) =>
          if (field.embedded) fromJson.fields.toVector :+ field.name
          else Vector(field.name)
        }

        override val fields: Set[String] = fieldNames.toSet

        override def read(jValue: JValue): JValidation[A] =
          jValue match {
            case jObject: JObject =>
              println(s"deriveCaseClass ${fields}")
              for {
                fieldsAsAList <- fieldsAndJsons
                  .map((field, fromJson) => readField(field, fromJson, jObject))
                  .sequence
                fieldsAsTuple = Tuple.fromArray(fieldsAsAList.toArray)

              } yield mirrorOfProduct.fromTuple(
                fieldsAsTuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

            case x =>
              Validated.invalidNel(JSONParseError(s"JSON object expected. $x"))
          }

        private def readField(
            field: Field,
            fromJson: FromJSON[Any],
            jObject: JObject): JValidation[Any] =
          if (field.embedded) fromJson.read(jObject)
          else io.sphere.json.field(field.fieldName, field.defaultArgument)(jObject)(fromJson)

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
