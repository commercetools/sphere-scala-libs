package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{FromJSON, JSON, JSONParseError, ToJSON}
import io.sphere.util.TraitMetaData
import org.json4s.DefaultJsonFormats.given
import org.json4s.{JObject, JString, jvalue2monadic, jvalue2readerSyntax}

object JSONTypeSwitch {
  import scala.compiletime.{erasedValue, error, summonInline}

  case class TraitInformation(
      serializedTypeNames: Map[String, String],
      traitFormatters: Map[String, JSON[Any]],
      caseClassFormatters: Map[String, JSON[Any]],
      traitMetaData: TraitMetaData
  )

  inline def readTraitInformation[SuperType, SubTypes <: Tuple]: TraitInformation = {
    val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
    val formattersAndMetaData = summonFormatters[SubTypes]()

    val (traitInTraitInfo, caseClassFormatters) =
      formattersAndMetaData.partitionMap { (meta, formatter) =>
        if (meta.isTrait) {
          val formatterByName = meta.subtypes.map((fieldName, m) => m.scalaName -> formatter)
          Left((formatterByName, meta.subTypeSerializedTypeNames))
        } else
          Right((meta.top.scalaName, formatter))
      }

    val (subTraitFormatters, subTraitSerializedTypeNames) = traitInTraitInfo.unzip
    // We could add some checks here to avoid the same type name in trait hierarchies
    val traitFormatters = subTraitFormatters.fold(Map.empty)(_ ++ _)
    val serializedTypeNames =
      subTraitSerializedTypeNames.fold(Map.empty)(
        _ ++ _) ++ traitMetaData.subTypeSerializedTypeNames

    TraitInformation(serializedTypeNames, traitFormatters, caseClassFormatters.toMap, traitMetaData)
  }

  inline def toJsonTypeSwitch[SuperType](info: TraitInformation): ToJSON[SuperType] =
    ToJSON.instance { a =>
      val scalaTypeName = a.asInstanceOf[Product].productPrefix
      val serializedTypeName = info.serializedTypeNames(scalaTypeName)
      val traitFormatterOpt = info.traitFormatters.get(scalaTypeName)
      traitFormatterOpt
        .map(_.write(a))
        .getOrElse(writeCaseClass(info, scalaTypeName, a, serializedTypeName))
    }

  inline def fromJsonTypeSwitch[SuperType](info: TraitInformation): FromJSON[SuperType] = {
    val scalaTypeNames = info.serializedTypeNames.map((on, n) => (n, on))
    val allFormattersByTypeName = info.traitFormatters ++ info.caseClassFormatters

    FromJSON.instance {
      case jObject: JObject =>
        val serializedTypeName = (jObject \ info.traitMetaData.typeDiscriminator).as[String]
        val scalaTypeName = scalaTypeNames(serializedTypeName)
        allFormattersByTypeName(scalaTypeName).read(jObject).map(_.asInstanceOf[SuperType])
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
      readFn = fromJson.read,
      subTypeNameList = info.serializedTypeNames.values.toList
    )
  }

  private def writeCaseClass[A](
      info: TraitInformation,
      scalaTypeName: String,
      a: A,
      serializedTypeName: String): JObject = {
    val jsonObj = info.caseClassFormatters(scalaTypeName).write(a) match {
      case JObject(obj) => obj
      case json =>
        throw new Exception(s"This code only handles objects as of now, but got: $json")
    }
    val typeDiscriminator = info.traitMetaData.typeDiscriminator -> JString(serializedTypeName)
    JObject(typeDiscriminator :: jsonObj)
  }

  inline private def summonFormatters[T <: Tuple](
      acc: Vector[(TraitMetaData, JSON[Any])] = Vector.empty): Vector[(TraitMetaData, JSON[Any])] =
    inline erasedValue[T] match {
      case _: EmptyTuple => acc
      case _: (t *: ts) =>
        val traitMetaData = AnnotationReader.readTraitMetaData[t]
        val headFormatter = summonInline[JSON[t]].asInstanceOf[JSON[Any]]
        summonFormatters[ts](acc :+ (traitMetaData, headFormatter))
    }

}
