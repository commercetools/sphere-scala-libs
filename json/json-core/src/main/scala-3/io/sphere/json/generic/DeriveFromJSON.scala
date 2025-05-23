package io.sphere.json.generic

import cats.data.Validated
import cats.syntax.traverse.*
import io.sphere.json.*
import io.sphere.util.{Field, TypeMetaData}
import org.json4s.JsonAST.*

import scala.deriving.Mirror

trait DeriveFromJSON {
  inline given derived[A](using Mirror.Of[A]): FromJSON[A] = Derivation.derived[A]

  protected object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): FromJSON[A] =
      inline m match {
        case s: Mirror.SumOf[A] => fromJsonTypeSwitch[A, s.MirroredElemTypes]
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): FromJSON[A] = {
      val caseClassMetaData: TypeMetaData = AnnotationReader.readTypeMetaData[A]
      val fromJsons: Vector[FromJSON[Any]] = summonFromJsons[mirrorOfProduct.MirroredElemTypes]
      val fieldsAndJsons: Vector[(Field, FromJSON[Any])] = caseClassMetaData.fields.zip(fromJsons)

      val fieldNames: Vector[String] = fieldsAndJsons.flatMap { (field, fromJson) =>
        if (field.embedded) fromJson.fields.toVector :+ field.scalaName
        else Vector(field.scalaName)
      }

      def readField(field: Field, fromJson: FromJSON[Any], jObject: JObject): JValidation[Any] =
        if (field.embedded) fromJson.read(jObject)
        else io.sphere.json.field(field.serializedName, field.defaultArgument)(jObject)(fromJson)

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
