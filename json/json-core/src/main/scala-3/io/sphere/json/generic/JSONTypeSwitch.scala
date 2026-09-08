package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{FromJSON, JSON, JSONParseError, ToJSON}
import org.json4s.{JObject, JString, jvalue2monadic}

import scala.reflect.ClassTag

object JSONTypeSwitch {
  import scala.compiletime.{erasedValue, summonInline}

  // The type discriminator field always comes from the top-level type, never from a subtype.
  // `reduce` skips `merge` when there is only one subtype, so stamp it explicitly.
  inline def deriveToFormatters[SuperType, SubTypes <: Tuple](
      typeDiscriminator: String): ToFormatters =
    summonToFormatters[SubTypes]()
      .reduce(ToFormatters.merge(typeDiscriminator))
      .copy(typeDiscriminator = typeDiscriminator)

  inline def deriveFromFormatters[SuperType, SubTypes <: Tuple](
      typeDiscriminator: String): FromFormatters =
    summonFromFormatters[SubTypes]()
      .reduce(FromFormatters.merge(typeDiscriminator))
      .copy(typeDiscriminator = typeDiscriminator)

  inline def toJsonTypeSwitch[SuperType](formatters: ToFormatters): ToJSON[SuperType] =
    ToJSON.instance(
      toJson = { scalaValue =>
        val clazz = scalaValue.getClass
        val serializedTypeName = formatters.serializedNamesByClass(clazz)
        val jsonObj = formatters.formatterByClass(clazz).write(scalaValue) match {
          case JObject(obj) => obj
          case json =>
            throw new Exception(s"This code only handles objects as of now, but got: $json")
        }
        // Match the Scala 2 behaviour: leave the discriminator untouched if the subtype's own
        // formatter already wrote it, could happen with handrolled formatters
        if (jsonObj.exists(_._1 == formatters.typeDiscriminator))
          JObject(jsonObj)
        else
          JObject(jsonObj :+ (formatters.typeDiscriminator -> JString(serializedTypeName)))
      },
      toFs = formatters
    )

  inline def fromJsonTypeSwitch[SuperType](formatters: FromFormatters): FromJSON[SuperType] =
    FromJSON.instance(
      readFn = {
        case jObject: JObject =>
          (jObject \ formatters.typeDiscriminator) match {
            case JString(serializedTypeName) =>
              formatters.formatterBySerializedName.get(serializedTypeName) match {
                case Some(formatter) =>
                  formatter.read(jObject).map(_.asInstanceOf[SuperType])
                case None =>
                  Validated.invalidNel(JSONParseError(
                    s"Invalid value '$serializedTypeName' for type field '${formatters.typeDiscriminator}'."))
              }
            case _ =>
              Validated.invalidNel(
                JSONParseError(s"Missing type field '${formatters.typeDiscriminator}'."))
          }
        case x =>
          Validated.invalidNel(JSONParseError(s"JSON object expected. Got: '$x'"))
      },
      fromFs = formatters
    )

  inline def jsonTypeSwitch[SuperType, SubTypes <: Tuple]: JSON[SuperType] = {
    val typeDiscriminator = AnnotationReader.readTraitMetaData[SuperType].typeDiscriminator
    val fromFormatters = deriveFromFormatters[SuperType, SubTypes](typeDiscriminator)
    val fromJson = fromJsonTypeSwitch[SuperType](fromFormatters)
    val toFormatters = deriveToFormatters[SuperType, SubTypes](typeDiscriminator)
    val toJson = toJsonTypeSwitch[SuperType](toFormatters)

    JSON.instance(
      writeFn = toJson.write,
      readFn = fromJson.read,
      subTypeNameList = fromFormatters.serializedNames,
      fromFs = fromJson.fromFormatters,
      toFs = toJson.toFormatters
    )
  }

  inline private def summonFromFormatters[T <: Tuple](
      acc: Vector[FromFormatters] = Vector.empty): Vector[FromFormatters] =
    inline erasedValue[T] match {
      case _: EmptyTuple => acc
      case _: (t *: ts) =>
        val traitMetaData = AnnotationReader.readTraitMetaData[t]
        val headFormatter = summonInline[FromJSON[t]].asInstanceOf[FromJSON[Any]]
        val (formatterMap, names) =
          if (traitMetaData.isTrait)
            (
              headFormatter.fromFormatters.formatterBySerializedName,
              headFormatter.fromFormatters.serializedNames)
          else
            (
              Map(traitMetaData.top.serializedName -> headFormatter),
              Vector(traitMetaData.top.serializedName)
            )

        val f = FromFormatters(
          serializedNames = names,
          formatterBySerializedName = formatterMap,
          typeDiscriminator = traitMetaData.typeDiscriminator
        )
        summonFromFormatters[ts](acc :+ f)
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
              formatterT.toFormatters.serializedNamesByClass
            )
          else {
            val clazz = summonInline[ClassTag[t]].runtimeClass
            (
              Map(clazz -> formatterT),
              Map(clazz -> traitMetaData.top.serializedName)
            )
          }

        val f = ToFormatters(
          serializedNamesByClass = serializedTypeNames,
          formatterByClass = formatterMap,
          typeDiscriminator = traitMetaData.typeDiscriminator
        )
        summonToFormatters[ts](acc :+ f)
    }

  case class ToFormatters(
      serializedNamesByClass: Map[Class[_], String],
      formatterByClass: Map[Class[_], ToJSON[Any]],
      typeDiscriminator: String
  )
  object ToFormatters {
    def merge(
        typeDiscriminatorFromParent: String)(f1: ToFormatters, f2: ToFormatters): ToFormatters =
      ToFormatters(
        serializedNamesByClass = f1.serializedNamesByClass ++ f2.serializedNamesByClass,
        formatterByClass = f1.formatterByClass ++ f2.formatterByClass,
        typeDiscriminator = typeDiscriminatorFromParent
      )
  }

  case class FromFormatters(
      serializedNames: Vector[String],
      formatterBySerializedName: Map[String, FromJSON[Any]],
      typeDiscriminator: String
  )

  object FromFormatters {
    def merge(typeDiscriminatorFromParent: String)(
        f1: FromFormatters,
        f2: FromFormatters): FromFormatters =
      FromFormatters(
        serializedNames = f1.serializedNames ++ f2.serializedNames,
        formatterBySerializedName = f1.formatterBySerializedName ++ f2.formatterBySerializedName,
        typeDiscriminator = typeDiscriminatorFromParent
      )
  }

}
