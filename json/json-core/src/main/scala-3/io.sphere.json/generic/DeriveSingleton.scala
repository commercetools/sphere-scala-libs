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
      inline m match {
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveObject(p)
      }

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): JSON[A] = {
      val traitMetaData: TraitMetaData = AnnotationReader.readTraitMetaData[A]

      val typeHintMap = traitMetaData.subTypeFieldRenames

      val reverseTypeHintMap: Map[String, String] = typeHintMap.map((on, n) => (n, on))
      val jsons: Seq[JSON[Any]] = summonFormatters[mirrorOfSum.MirroredElemTypes]

      val names: Seq[String] =
        constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
          .asInstanceOf[Vector[String]]

      val jsonsByNames: Map[String, JSON[Any]] = names.zip(jsons).toMap

      JSON.instance(
        readFn = {
          case JString(typeName) =>
            val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
            jsonsByNames.get(originalTypeName) match {
              case Some(json) =>
                json.read(JNull).map(_.asInstanceOf[A])
              case None =>
                Validated.invalidNel(JSONParseError(s"'$typeName' is not a valid value"))
            }

          case x =>
            Validated.invalidNel(JSONParseError(s"JSON string expected. Got >>> $x"))
        },
        writeFn = { value =>
          val originalTypeName = value.asInstanceOf[Product].productPrefix
          val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
          JString(typeName)
        }
      )
    }

    inline private def deriveObject[A](mirrorOfProduct: Mirror.ProductOf[A]): JSON[A] =
      JSON.instance(
        writeFn = { _ => ??? }, // This is already taken care of in `deriveTrait`
        readFn = { _ =>
          // Just create the object instance, no need to do anything else
          val tuple = Tuple.fromArray(Array.empty[Any])
          val obj = mirrorOfProduct.fromTuple(tuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])
          Validated.Valid(obj)
        }
      )

    inline private def summonFormatters[T <: Tuple]: Vector[JSON[Any]] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[JSON[t]]
            .asInstanceOf[JSON[Any]] +: summonFormatters[ts]
      }
  }
}
