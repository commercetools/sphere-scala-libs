package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{JSON, JSONParseError, JValidation}
import org.json4s.{JNull, JString, JValue}

import scala.deriving.Mirror

inline def deriveSingletonJSON[A](using Mirror.Of[A]): JSON[A] = DeriveSingleton.derived

object DeriveSingleton {

  inline given derived[A](using Mirror.Of[A]): JSON[A] = Derivation.derived[A]

  private object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): JSON[A] =
      inline m match
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveObject(p)

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): JSON[A] =
      new JSON[A]:
        private val traitMetaData: TraitMetaData = AnnotationReader.readTraitMetaData[A]
        private val typeHintMap: Map[String, String] = traitMetaData.subtypes.collect {
          case (name, classMeta) if classMeta.typeHint.isDefined =>
            name -> classMeta.typeHint.get
        }
        private val reverseTypeHintMap: Map[String, String] = typeHintMap.map((on, n) => (n, on))
        private val jsons: Seq[JSON[Any]] = summonFormatters[mirrorOfSum.MirroredElemTypes]
        private val names: Seq[String] =
          constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
            .asInstanceOf[Vector[String]]
        private val jsonsByNames: Map[String, JSON[Any]] = names.zip(jsons).toMap

        override def read(jValue: JValue): JValidation[A] =
          jValue match
            case JString(typeName) =>
              val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
              jsonsByNames.get(originalTypeName) match
                case Some(json) =>
                  json.read(JNull).map(_.asInstanceOf[A])
                case None =>
                  Validated.invalidNel(JSONParseError(s"'$typeName' is not a valid value"))

            case x =>
              Validated.invalidNel(JSONParseError(s"JSON string expected. Got >>> $jValue"))

        override def write(value: A): JValue =
          val originalTypeName = value.asInstanceOf[Product].productPrefix
          val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
          JString(typeName)

    end deriveTrait

    inline private def deriveObject[A](mirrorOfProduct: Mirror.ProductOf[A]): JSON[A] =
      new JSON[A]:
        override def write(value: A): JValue = ??? // This is already taken care of in `deriveTrait`
        override def read(jValue: JValue): JValidation[A] =
          // Just create the object instance, no need to do anything else
          val tuple = Tuple.fromArray(Array.empty[Any])
          val obj = mirrorOfProduct.fromTuple(tuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])
          Validated.Valid(obj)
    end deriveObject

    inline private def summonFormatters[T <: Tuple]: Vector[JSON[Any]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[JSON[t]]
            .asInstanceOf[JSON[Any]] +: summonFormatters[ts]

  }

}
