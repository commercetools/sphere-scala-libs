package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{FromJSON, JSONParseError, ToJSON}
import org.json4s.{JObject, JString, jvalue2monadic}

object JSONTypeSwitch {

  def toJsonTypeSwitch[SuperType](formatters: ToFormatters): ToJSON[SuperType] =
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

  def fromJsonTypeSwitch[SuperType](formatters: FromFormatters): FromJSON[SuperType] =
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

  case class ToFormatters(
      serializedNamesByClass: Map[Class[_], String],
      formatterByClass: Map[Class[_], ToJSON[Any]],
      typeDiscriminator: String
  )
  object ToFormatters {
    // The type discriminator is not merged: the switch stamps the top-level type's one
    // afterwards, because `reduce` skips `merge` entirely for a single subtype.
    def merge(f1: ToFormatters, f2: ToFormatters): ToFormatters =
      ToFormatters(
        serializedNamesByClass = f1.serializedNamesByClass ++ f2.serializedNamesByClass,
        formatterByClass = f1.formatterByClass ++ f2.formatterByClass,
        typeDiscriminator = f1.typeDiscriminator
      )
  }

  case class FromFormatters(
      serializedNames: Vector[String],
      formatterBySerializedName: Map[String, FromJSON[Any]],
      typeDiscriminator: String
  )

  object FromFormatters {
    def merge(f1: FromFormatters, f2: FromFormatters): FromFormatters =
      FromFormatters(
        serializedNames = f1.serializedNames ++ f2.serializedNames,
        formatterBySerializedName = f1.formatterBySerializedName ++ f2.formatterBySerializedName,
        typeDiscriminator = f1.typeDiscriminator
      )
  }

}
