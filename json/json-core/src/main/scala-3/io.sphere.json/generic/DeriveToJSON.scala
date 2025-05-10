package io.sphere.json.generic

import io.sphere.json.ToJSON
import org.json4s.JsonAST.*

import scala.deriving.Mirror

trait DeriveToJSON {

  inline given derived[A](using Mirror.Of[A]): ToJSON[A] = Derivation.derived[A]

  protected object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): ToJSON[A] =
      inline m match {
        case s: Mirror.SumOf[A] => toJsonTypeSwitch[A, s.MirroredElemTypes]
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): ToJSON[A] = {
      val caseClassMetaData: TypeMetaData = AnnotationReader.readTypeMetaData[A]
      val toJsons: Vector[ToJSON[Any]] = summonToJson[mirrorOfProduct.MirroredElemTypes]

      ToJSON.instance { value =>
        val caseClassFields = value.asInstanceOf[Product].productIterator
        toJsons.iterator
          .zip(caseClassFields)
          .zip(caseClassMetaData.fields)
          .foldLeft[JValue](JObject()) { case (jObject, ((toJson, fieldValue), field)) =>
            addField(jObject.asInstanceOf[JObject], field, toJson.write(fieldValue))
          }
      }
    }

    inline private def summonToJson[T <: Tuple]: Vector[ToJSON[Any]] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[ToJSON[t]]
            .asInstanceOf[ToJSON[Any]] +: summonToJson[ts]
      }
  }

  private def addField(jObject: JObject, field: Field, jValue: JValue): JValue =
    jValue match {
      case o: JObject =>
        if (field.embedded) JObject(jObject.obj ++ o.obj)
        else JObject(jObject.obj :+ (field.fieldName -> o))
      case other => JObject(jObject.obj :+ (field.fieldName -> other))
    }

}
