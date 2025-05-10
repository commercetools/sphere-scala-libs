package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{FromJSON, JSON, JSONParseError, ToJSON}
import io.sphere.util.TraitMetaData
import org.json4s.{JObject, JString, jvalue2monadic, jvalue2readerSyntax}
import org.json4s.DefaultJsonFormats.given

object JSONTypeSwitch {
  import scala.compiletime.{erasedValue, error, summonInline}

  case class TraitInformation(
      mergedTypeHintMap: Map[String, String],
      traitInTraitFormatterMap: Map[String, JSON[Any]],
      caseClassFormatterMap: Map[String, JSON[Any]],
      traitMetaData: TraitMetaData)

  inline def readTraitInformation[SuperType, SubTypes <: Tuple]: TraitInformation = {
    val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
    val formattersAndMetaData: Vector[(TraitMetaData, JSON[Any])] = summonFormatters[SubTypes]()

    // - Separate Trait formatters from CaseClass formatters, so we can avoid adding the typeDiscriminator twice
    // - Currently we support 2 layers of traits, because the AnnotationReader only tries to read to 2 levels
    val (traitInTraitInfo, caseClassFormatters) =
      formattersAndMetaData.partitionMap { (meta, formatter) =>
        if (meta.isTrait) {
          val formatterByName = meta.subtypes.map((fieldName, m) => m.scalaName -> formatter)
          Left(formatterByName -> meta.subTypeFieldRenames)
        } else {
          Right(meta.top.scalaName -> formatter)
        }
      }

    val (traitInTraitFormatters, traitInTraitRenames) = traitInTraitInfo.unzip
    val traitInTraitFormatterMap = traitInTraitFormatters.fold(Map.empty)(_ ++ _)

    val caseClassFormatterMap = caseClassFormatters.toMap

    val mergedTypeHintMap =
      traitMetaData.subTypeFieldRenames ++ traitInTraitRenames.fold(Map.empty)(_ ++ _)

    TraitInformation(
      mergedTypeHintMap,
      traitInTraitFormatterMap,
      caseClassFormatterMap,
      traitMetaData)
  }

  inline def toJsonTypeSwitch[SuperType](info: TraitInformation): ToJSON[SuperType] =
    ToJSON.instance { a =>
      val originalTypeName = a.asInstanceOf[Product].productPrefix
      val typeName = info.mergedTypeHintMap.getOrElse(originalTypeName, originalTypeName)
      val traitFormatterOpt = info.traitInTraitFormatterMap.get(originalTypeName)
      traitFormatterOpt
        .map(_.write(a))
        .getOrElse {
          val jsonObj = info.caseClassFormatterMap(originalTypeName).write(a) match {
            case JObject(obj) => obj
            case json =>
              throw new Exception(s"This code only handles objects as of now, but got: $json")
          }
          val typeDiscriminator = info.traitMetaData.typeDiscriminator -> JString(typeName)
          JObject(typeDiscriminator :: jsonObj)
        }
    }

  inline def fromJsonTypeSwitch[SuperType](info: TraitInformation): FromJSON[SuperType] = {
    val reverseTypeHintMap = info.mergedTypeHintMap.map((on, n) => (n, on))
    val allFormattersByTypeName = info.traitInTraitFormatterMap ++ info.caseClassFormatterMap

    FromJSON.instance {
      case jObject: JObject =>
        val typeName = (jObject \ info.traitMetaData.typeDiscriminator).as[String]
        val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
        allFormattersByTypeName(originalTypeName).read(jObject).map(_.asInstanceOf[SuperType])
      case x =>
        Validated.invalidNel(JSONParseError(s"JSON object expected. Got: '$x'"))
    }
  }

  inline def jsonTypeSwitch[SuperType, SubTypes <: Tuple]: JSON[SuperType] = {
    val info = readTraitInformation[SuperType, SubTypes]
    val fromJson = fromJsonTypeSwitch[SuperType](info)
    val toJson = toJsonTypeSwitch[SuperType](info)

    JSON.instance(
      writeFn = toJson.write,
      readFn = fromJson.read
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

}
