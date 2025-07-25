package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{FromJSON, JSON, JSONParseError, ToJSON}
import org.json4s.DefaultJsonFormats.given
import org.json4s.{JObject, JString, jvalue2monadic, jvalue2readerSyntax}

import scala.compiletime.{constValue, constValueTuple}
import scala.reflect.ClassTag

object JSONTypeSwitch {
  import scala.compiletime.{erasedValue, summonInline}

  inline def deriveToFormatters[SuperType, SubTypes <: Tuple]: ToFormatters = {
    val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
    summonToFormatters[SubTypes]()
      .reduce(ToFormatters.merge)
      .copy(typeDiscriminator = traitMetaData.typeDiscriminator)
  }

  inline def deriveFromFormatters[SuperType, SubTypes <: Tuple]: FromFormatters = {
    val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
    summonFromFormatters[SubTypes]()
      .reduce(FromFormatters.merge)
      .copy(typeDiscriminator = traitMetaData.typeDiscriminator)
  }

  inline def toJsonTypeSwitch[SuperType](formatters: ToFormatters): ToJSON[SuperType] =
    ToJSON.instance(
      toJson = { scalaValue =>
        val clazz = scalaValue.getClass
        val serializedTypeName = formatters.serializedTypeNames(clazz)
        val jsonObj = formatters.formatterByClass(clazz).write(scalaValue) match {
          case JObject(obj) => obj
          case json =>
            throw new Exception(s"This code only handles objects as of now, but got: $json")
        }
        val typeDiscriminator = formatters.typeDiscriminator -> JString(serializedTypeName)
        JObject(typeDiscriminator :: jsonObj)
      },
      toFs = formatters
    )

  inline def fromJsonTypeSwitch[SuperType](formatters: FromFormatters): FromJSON[SuperType] =
    FromJSON.instance(
      readFn = {
        case jObject: JObject =>
          val serializedTypeName = (jObject \ formatters.typeDiscriminator).as[String]
          val scalaTypeName = formatters.scalaNamesFromSerializedNames(serializedTypeName)
          formatters
            .formatterByScalaName(scalaTypeName)
            .read(jObject)
            .map(_.asInstanceOf[SuperType])
        case x =>
          Validated.invalidNel(JSONParseError(s"JSON object expected. Got: '$x'"))
      },
      fromFs = formatters
    )

  inline def jsonTypeSwitch[SuperType, SubTypes <: Tuple]: JSON[SuperType] = {
    val fromFormatters = deriveFromFormatters[SuperType, SubTypes]
    val fromJson = fromJsonTypeSwitch[SuperType](fromFormatters)
    val toFormatters = deriveToFormatters[SuperType, SubTypes]
    val toJson = toJsonTypeSwitch[SuperType](toFormatters)

    JSON.instance(
      writeFn = toJson.write,
      readFn = fromJson.read,
      subTypeNameList = fromFormatters.getSerializedNames,
      fromFs = fromJson.fromFormatters,
      toFs = toJson.toFormatters
    )
  }

  inline private def summonFromFormatters[T <: Tuple](
      d: Int = 0,
      acc: Vector[FromFormatters] = Vector.empty): Vector[FromFormatters] =
    inline erasedValue[T] match {
      case _: EmptyTuple => acc
      case _: (t *: ts) =>
        val traitMetaData = AnnotationReader.readTraitMetaData[t]
        val headFormatter = summonInline[FromJSON[t]].asInstanceOf[FromJSON[Any]]
        val (formatterMap, nameMap) =
          if (traitMetaData.isTrait)
            (
              headFormatter.fromFormatters.formatterByScalaName,
              headFormatter.fromFormatters.scalaNamesFromSerializedNames)
          else
            (
              Map(traitMetaData.top.scalaName -> headFormatter),
              Map(traitMetaData.top.serializedName -> traitMetaData.top.scalaName)
            )

        val f = FromFormatters(
          scalaNamesFromSerializedNames = nameMap,
          formatterByScalaName = formatterMap,
          typeDiscriminator = traitMetaData.typeDiscriminator
        )
        summonFromFormatters[ts](d + 1, acc :+ f)
    }

  inline private def summonToFormatters[T <: Tuple](
      acc: Vector[ToFormatters] = Vector.empty): Vector[ToFormatters] =
    inline erasedValue[T] match {
      case _: EmptyTuple => acc
      case _: (t *: ts) =>
        val traitMetaData = AnnotationReader.readTraitMetaData[t]
        val formatterT = summonInline[ToJSON[t]].asInstanceOf[ToJSON[Any]]

        val (formatterMap, serializedTypeNames) =
          if (traitMetaData.isTrait)
            (
              formatterT.toFormatters.formatterByClass,
              formatterT.toFormatters.serializedTypeNames
            )
          else {
            val clazz = summonInline[ClassTag[t]].runtimeClass
            (
              Map(clazz -> formatterT),
              Map(clazz -> traitMetaData.top.serializedName)
            )
          }

        val f = ToFormatters(
          serializedTypeNames = serializedTypeNames,
          formatterByClass = formatterMap,
          typeDiscriminator = traitMetaData.typeDiscriminator
        )
        summonToFormatters[ts](acc :+ f)
    }

  case class ToFormatters(
      serializedTypeNames: Map[Class[_], String],
      formatterByClass: Map[Class[_], ToJSON[Any]],
      typeDiscriminator: String
  )
  object ToFormatters {
    def merge(f1: ToFormatters, f2: ToFormatters): ToFormatters = {
      require(
        f1.typeDiscriminator == f2.typeDiscriminator,
        "@JSONTypeHintField has to be the same on all traits")
      ToFormatters(
        serializedTypeNames = f1.serializedTypeNames ++ f2.serializedTypeNames,
        formatterByClass = f1.formatterByClass ++ f2.formatterByClass,
        typeDiscriminator = f1.typeDiscriminator
      )
    }
  }

  case class FromFormatters(
      scalaNamesFromSerializedNames: Map[String, String],
      formatterByScalaName: Map[String, FromJSON[Any]],
      typeDiscriminator: String
  ) {
    def getSerializedNames: List[String] = scalaNamesFromSerializedNames.keys.toList
  }

  object FromFormatters {
    def merge(f1: FromFormatters, f2: FromFormatters): FromFormatters = {
      require(
        f1.typeDiscriminator == f2.typeDiscriminator,
        "@JSONTypeHintField has to be the same on all traits")
      FromFormatters(
        scalaNamesFromSerializedNames =
          f1.scalaNamesFromSerializedNames ++ f2.scalaNamesFromSerializedNames,
        formatterByScalaName = f1.formatterByScalaName ++ f2.formatterByScalaName,
        typeDiscriminator = f1.typeDiscriminator
      )
    }
  }

}
