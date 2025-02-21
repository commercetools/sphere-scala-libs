package io.sphere.json.generic

import cats.data.Validated
import io.sphere.json.{JSON, JSONParseError, JValidation}
import org.json4s.DefaultJsonFormats.given
import org.json4s.{JObject, JString, jvalue2monadic, jvalue2readerSyntax}
import org.json4s.JsonAST.JValue

import scala.compiletime.{erasedValue, error, summonInline}

inline def jsonTypeSwitch[SuperType, SubTypeTuple <: Tuple](): JSON[SuperType] =
  new JSON[SuperType] {
    private val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
    private val typeHintMap = traitMetaData.subTypeTypeHints
    private val reverseTypeHintMap = typeHintMap.map((on, n) => (n, on))
    private val formattersAndMetaData: Vector[(TraitMetaData, JSON[Any])] =
      summonFormatters[SubTypeTuple]()

    // Separate Trait formatters from CaseClass formatters, so we can avoid adding the typeDiscriminator twice
    private val (traitFormatterList, caseClassFormatterList) =
      formattersAndMetaData.partitionMap { (meta, formatter) =>
        if (meta.isTrait)
          Left(meta.subtypes.map(_._2.name -> formatter))
        else
          Right(meta.top.name -> formatter)
      }
    val traitFormatters = traitFormatterList.flatten.toMap
    val caseClassFormatters = caseClassFormatterList.toMap
    val allFormattersByTypeName = traitFormatters ++ caseClassFormatters

    override def write(a: SuperType): JValue = {
      val originalTypeName = a.asInstanceOf[Product].productPrefix
      val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
      val traitFormatterOpt = traitFormatters.get(originalTypeName)
      traitFormatterOpt
        .map(_.write(a).asInstanceOf[JObject])
        .getOrElse {
          val json = caseClassFormatters(originalTypeName).write(a).asInstanceOf[JObject]
          val typeDiscriminator = traitMetaData.typeDiscriminator -> JString(typeName)
          JObject(typeDiscriminator :: json.obj)
        }
    }

    override def read(jValue: JValue): JValidation[SuperType] =
      jValue match {
        case jObject: JObject =>
          val typeName = (jObject \ traitMetaData.typeDiscriminator).as[String]
          val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
          allFormattersByTypeName(originalTypeName).read(jObject).map(_.asInstanceOf[SuperType])
        case x =>
          Validated.invalidNel(JSONParseError(s"JSON object expected. Got: '$jValue'"))
      }
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
