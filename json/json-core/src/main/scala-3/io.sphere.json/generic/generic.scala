package io.sphere.json.generic

import cats.data.Validated
import cats.syntax.validated.*
import io.sphere.json.{
  FromJSON,
  JSON,
  JSONError,
  JSONParseError,
  JValidation,
  ToJSON,
  jsonParseError,
  toJSON,
  toJValue
}
import org.json4s.DefaultJsonFormats.given
import org.json4s.{JObject, JString, jvalue2monadic, jvalue2readerSyntax}
import org.json4s.JsonAST.JValue

import scala.collection.mutable
import scala.compiletime.{erasedValue, error, summonInline}

/** Creates a ToJSON instance for an Enumeration type that encodes the `toString` representations of
  * the enumeration values.
  */
def toJsonEnum(e: Enumeration): ToJSON[e.Value] = new ToJSON[e.Value] {
  def write(a: e.Value): JValue = JString(a.toString)
}

/** Creates a FromJSON instance for an Enumeration type that encodes the `toString` representations
  * of the enumeration values.
  */
def fromJsonEnum(e: Enumeration): FromJSON[e.Value] = {
  // We're using an AnyRefMap for performance, it's not for mutability.
  val validRepresentations = mutable.AnyRefMap(
    e.values.iterator.map { value =>
      value.toString -> value.validNel[JSONError]
    }.toSeq: _*
  )

  val allowedValues = e.values.mkString("'", "','", "'")

  new FromJSON[e.Value] {
    override def read(json: JValue): JValidation[e.Value] =
      json match {
        case JString(string) =>
          validRepresentations.getOrElse(
            string,
            jsonParseError(
              "Invalid enum value: '%s'. Expected one of: %s".format(string, allowedValues)))
        case _ => jsonParseError("JSON String expected.")
      }
  }
}

// This can be used instead of deriveJSON
def jsonEnum(e: Enumeration): JSON[e.Value] = {
  val toJson = toJsonEnum(e)
  val fromJson = fromJsonEnum(e)

  new JSON[e.Value] {
    override def read(jval: JValue): JValidation[e.Value] = fromJson.read(jval)

    override def write(value: e.Value): JValue = toJson.write(value)
  }
}

inline def jsonTypeSwitch[SuperType, SubTypeTuple <: Tuple](): JSON[SuperType] = {
  val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
  val formattersAndMetaData: Vector[(TraitMetaData, JSON[Any])] = summonFormatters[SubTypeTuple]()

  // Separate Trait formatters from CaseClass formatters, so we can avoid adding the typeDiscriminator twice
  val (traitFormatterList, caseClassFormatterList) =
    formattersAndMetaData.partitionMap { (meta, formatter) =>
      if (meta.isTrait)
        Left(meta.subtypes.map(_._2.name -> formatter))
      else
        Right(meta.top.name -> formatter)
    }

  val traitFormatters = traitFormatterList.flatten.toMap
  val caseClassFormatters = caseClassFormatterList.toMap
  val allFormattersByTypeName = traitFormatters ++ caseClassFormatters

  // We could add some checking here to filter duplicate keys
  val subTypeHints =
    traitFormatters.map((_, formatter) => formatter.traitTypeHintMap).fold(Map.empty)(_ ++ _)
  val mergedTypeHintMap = traitMetaData.subTypeTypeHints ++ subTypeHints
  val reverseTypeHintMap = mergedTypeHintMap.map((on, n) => (n, on))

  JSON.instance(
    writeFn = { a =>
      val originalTypeName = a.asInstanceOf[Product].productPrefix
      val typeName = mergedTypeHintMap.getOrElse(originalTypeName, originalTypeName)
      val traitFormatterOpt = traitFormatters.get(originalTypeName)
      traitFormatterOpt
        .map(_.write(a).asInstanceOf[JObject])
        .getOrElse {
          val json = caseClassFormatters(originalTypeName).write(a).asInstanceOf[JObject]
          val typeDiscriminator = traitMetaData.typeDiscriminator -> JString(typeName)
          JObject(typeDiscriminator :: json.obj)
        }
    },
    readFn = {
      case jObject: JObject =>
        val typeName = (jObject \ traitMetaData.typeDiscriminator).as[String]
        val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
        allFormattersByTypeName(originalTypeName).read(jObject).map(_.asInstanceOf[SuperType])
      case x =>
        Validated.invalidNel(JSONParseError(s"JSON object expected. Got: '$x'"))
    }
  )
}

inline private def summonFormatters[T <: Tuple](
    acc: Vector[(TraitMetaData, JSON[Any])] = Vector.empty): Vector[(TraitMetaData, JSON[Any])] =
  inline erasedValue[T] match {
    case _: EmptyTuple => acc
    case _: (t *: ts) =>
      val traitMetaData = AnnotationReader.readTraitMetaData[t]
      val headFormatter = summonInline[JSON[t]].asInstanceOf[JSON[Any]]
      summonFormatters[ts](acc :+ (traitMetaData -> headFormatter))
  }
