package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{FromJSON, JSON, JSONParseError, ToJSON}
import org.json4s.DefaultJsonFormats.given
import org.json4s.{JObject, JString, jvalue2monadic, jvalue2readerSyntax}

import scala.compiletime.{constValue, constValueTuple}

object JSONTypeSwitch {
  import scala.compiletime.{erasedValue, summonInline}

  case class Formatters[JsonF[_]](
      serializedTypeNames: Map[String, String],
      forCaseClasses: Map[String, JsonF[Any]],
      typeDiscriminator: String
  ) {
    def addTypeNames(names: Map[String, String]): Formatters[JsonF] =
      copy(serializedTypeNames = serializedTypeNames ++ names)
  }

  object Formatters {
    def merge[JsonF[_]](f1: Formatters[JsonF], f2: Formatters[JsonF]): Formatters[JsonF] =
      Formatters[JsonF](
        serializedTypeNames = f1.serializedTypeNames ++ f2.serializedTypeNames,
        forCaseClasses = f1.forCaseClasses ++ f2.forCaseClasses,
        typeDiscriminator = f1.typeDiscriminator
      )
  }

  inline def deriveToFormatters[SuperType, SubTypes <: Tuple]: Formatters[ToJSON] = {
    val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
    summonToFormatters[SubTypes]()
      .reduce(Formatters.merge)
      .copy(typeDiscriminator = traitMetaData.typeDiscriminator)
      .addTypeNames(traitMetaData.subTypeSerializedTypeNames)
  }

  inline def deriveFromFormatters[SuperType, SubTypes <: Tuple]: Formatters[FromJSON] = {
    val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
    summonFromFormatters[SubTypes]()
      .reduce(Formatters.merge)
      .copy(typeDiscriminator = traitMetaData.typeDiscriminator)
      .addTypeNames(traitMetaData.subTypeSerializedTypeNames)
  }

  inline def toJsonTypeSwitch[SuperType](formatters: Formatters[ToJSON]): ToJSON[SuperType] =
    ToJSON.instance(formatters) { a =>
      val scalaTypeName = a.asInstanceOf[Product].productPrefix
      val serializedTypeName = formatters.serializedTypeNames(scalaTypeName)
      val jsonObj = formatters.forCaseClasses(scalaTypeName).write(a) match {
        case JObject(obj) => obj
        case json =>
          throw new Exception(s"This code only handles objects as of now, but got: $json")
      }
      val typeDiscriminator = formatters.typeDiscriminator -> JString(serializedTypeName)
      JObject(typeDiscriminator :: jsonObj)
    }

  inline def fromJsonTypeSwitch[SuperType](
      formatters: Formatters[FromJSON]): FromJSON[SuperType] = {
    val scalaTypeNames = formatters.serializedTypeNames.map((on, n) => (n, on))

    FromJSON.instance(
      readFn = {
        case jObject: JObject =>
          val serializedTypeName = (jObject \ formatters.typeDiscriminator).as[String]
          val scalaTypeName = scalaTypeNames(serializedTypeName)
          formatters.forCaseClasses(scalaTypeName).read(jObject).map(_.asInstanceOf[SuperType])
        case x =>
          Validated.invalidNel(JSONParseError(s"JSON object expected. Got: '$x'"))
      },
      fromFs = formatters
    )
  }

  inline def jsonTypeSwitch[SuperType, SubTypes <: Tuple]: JSON[SuperType] = {
    val fromFormatters = deriveFromFormatters[SuperType, SubTypes]
    val fromJson = fromJsonTypeSwitch[SuperType](fromFormatters)
    val toFormatters = deriveToFormatters[SuperType, SubTypes]
    val toJson = toJsonTypeSwitch[SuperType](toFormatters)

    JSON.instance(
      writeFn = toJson.write,
      readFn = fromJson.read,
      subTypeNameList = fromFormatters.serializedTypeNames.values.toList,
      fromFs = fromJson.fromFormatters,
      toFs = toJson.toFormatters
    )
  }

  inline private def summonFromFormatters[T <: Tuple](
      d: Int = 0,
      acc: Vector[Formatters[FromJSON]] = Vector.empty): Vector[Formatters[FromJSON]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => acc
      case _: (t *: ts) =>
        val traitMetaData = AnnotationReader.readTraitMetaData[t]
        val headFormatter = summonInline[FromJSON[t]].asInstanceOf[FromJSON[Any]]
        val formatterMap =
          if (traitMetaData.isTrait) {
            headFormatter.fromFormatters.forCaseClasses
          } else
            Map(traitMetaData.top.scalaName -> headFormatter)

        val f = Formatters[FromJSON](
          serializedTypeNames = traitMetaData.subTypeSerializedTypeNames,
          forCaseClasses = formatterMap,
          typeDiscriminator = traitMetaData.typeDiscriminator
        )
        summonFromFormatters[ts](d + 1, acc :+ f)
    }

  inline private def summonToFormatters[T <: Tuple](
      acc: Vector[Formatters[ToJSON]] = Vector.empty): Vector[Formatters[ToJSON]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => acc
      case _: (t *: ts) =>
        val traitMetaData = AnnotationReader.readTraitMetaData[t]
        val headFormatter = summonInline[ToJSON[t]].asInstanceOf[ToJSON[Any]]
        val formatterMap =
          if (traitMetaData.isTrait)
            headFormatter.toFormatters.forCaseClasses
          else
            Map(traitMetaData.top.scalaName -> headFormatter)

        val f = Formatters[ToJSON](
          serializedTypeNames = traitMetaData.subTypeSerializedTypeNames,
          forCaseClasses = formatterMap,
          typeDiscriminator = traitMetaData.typeDiscriminator
        )
        summonToFormatters[ts](acc :+ f)
    }

}
