package io.sphere.json.generic

import io.sphere.json.ToJSON
import org.json4s.JsonAST.*

import scala.deriving.Mirror

trait DeriveToJSON {

  inline given derived[A](using Mirror.Of[A]): ToJSON[A] = Derivation.derived[A]

  private def addField(jObject: JObject, field: Field, jValue: JValue): JValue =
    jValue match {
      case o: JObject =>
        if (field.embedded) JObject(jObject.obj ++ o.obj)
        else JObject(jObject.obj :+ (field.fieldName -> o))
      case other => JObject(jObject.obj :+ (field.fieldName -> other))
    }

  protected object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): ToJSON[A] =
      inline m match {
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): ToJSON[A] =
      new ToJSON[A] {
        private val traitMetaData: TraitMetaData = AnnotationReader.readTraitMetaData[A]
        private val typeHintMap: Map[String, String] = traitMetaData.subtypes.collect {
          case (name, classMeta) if classMeta.typeHint.isDefined =>
            name -> classMeta.typeHint.get
        }
        private val reverseTypeHintMap: Map[String, String] = typeHintMap.map((on, n) => (n, on))
        private val jsons: Seq[ToJSON[Any]] = summonToJson[mirrorOfSum.MirroredElemTypes]
        private val names: Seq[String] =
          constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
            .asInstanceOf[Vector[String]]
        private val jsonsByNames: Map[String, ToJSON[Any]] = names.zip(jsons).toMap

        override def write(value: A): JValue = {
          // we never get a trait here, only classes, it's safe to assume Product
          val originalTypeName = value.asInstanceOf[Product].productPrefix
          val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
          val json = jsonsByNames(originalTypeName).write(value).asInstanceOf[JObject]
          val typeDiscriminator = traitMetaData.typeDiscriminator -> JString(typeName)
          JObject(typeDiscriminator :: json.obj)
        }

      }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): ToJSON[A] =
      new ToJSON[A] {
        private val caseClassMetaData: CaseClassMetaData = AnnotationReader.readCaseClassMetaData[A]
        private val toJsons: Vector[ToJSON[Any]] = summonToJson[mirrorOfProduct.MirroredElemTypes]

        override def write(value: A): JValue = {
          val caseClassFields = value.asInstanceOf[Product].productIterator
          toJsons
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

}
