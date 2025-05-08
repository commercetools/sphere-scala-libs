package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{JSON, JSONParseError, JValidation}
import org.json4s.{JNull, JString, JValue}

import scala.deriving.Mirror

inline def deriveSingletonJSON[A](using Mirror.Of[A]): DeriveSingleton[A] = DeriveSingleton.derived

// This is required so we don't summon normal JSON instances (maybe there's a better way to work around this)
trait DeriveSingleton[A] extends JSON[A]

object DeriveSingleton {
  def instance[A](
      readFn: JValue => JValidation[A],
      writeFn: A => JValue,
      fieldSet: Set[String] = Set.empty): DeriveSingleton[A] = new {

    override def read(jval: JValue): JValidation[A] = readFn(jval)
    override def write(value: A): JValue = writeFn(value)
    override val fields: Set[String] = fieldSet
  }
  inline given derived[A](using Mirror.Of[A]): DeriveSingleton[A] = Derivation.derived[A]

  private object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): DeriveSingleton[A] =
      inline m match {
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveObject(p)
      }

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): DeriveSingleton[A] = {
      val traitMetaData: TraitMetaData = AnnotationReader.readTraitMetaData[A]

      val typeHintMap = traitMetaData.subTypeFieldRenames

      val reverseTypeHintMap: Map[String, String] = typeHintMap.map((on, n) => (n, on))
      val jsons: Seq[DeriveSingleton[Any]] = summonFormatters[mirrorOfSum.MirroredElemTypes]

      val names: Seq[String] =
        constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
          .asInstanceOf[Vector[String]]

      val jsonsByNames: Map[String, DeriveSingleton[Any]] = names.zip(jsons).toMap

      DeriveSingleton.instance(
        readFn = {
          case JString(typeName) =>
            val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
            jsonsByNames.get(originalTypeName) match {
              case Some(json) =>
                val dummyValue = JNull
                json.read(dummyValue).map(_.asInstanceOf[A])
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

    inline private def deriveObject[A](mirrorOfProduct: Mirror.ProductOf[A]): DeriveSingleton[A] =
      DeriveSingleton.instance(
        writeFn = { _ => ??? }, // This is already taken care of in `deriveTrait`
        readFn = { _ =>
          // Just create the object instance, no need to do anything else
          val obj =
            mirrorOfProduct.fromTuple(EmptyTuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])
          Validated.Valid(obj)
        }
      )

    inline private def summonFormatters[T <: Tuple]: Vector[DeriveSingleton[Any]] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[DeriveSingleton[t]]
            .asInstanceOf[DeriveSingleton[Any]] +: summonFormatters[ts]
      }
  }
}
