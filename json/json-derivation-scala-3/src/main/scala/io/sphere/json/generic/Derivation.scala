package io.sphere.json.generic

import cats.data.Validated
import cats.implicits.*
import io.sphere.json.{JSON, JSONParseError, JValidation}
import org.json4s.DefaultJsonFormats.given
import org.json4s.JsonAST.JValue
import org.json4s.{DefaultJsonFormats, JNull, JObject, JString, jvalue2monadic, jvalue2readerSyntax}

import scala.deriving.Mirror
import scala.quoted.*

inline def deriveJSON[A](using Mirror.Of[A]): JSON[A] = JSON.derived

object JSON:
  private val emptyFieldsSet: Vector[String] = Vector.empty

  inline def apply[A: JSON]: JSON[A] = summon[JSON[A]]
  inline given derived[A](using Mirror.Of[A]): JSON[A] = Derivation.derived[A]
  private inline def readCaseClassMetaData[T]: CaseClassMetaData = ${ readCaseClassMetaDataImpl[T] }
  private inline def readTraitMetaData[T]: TraitMetaData = ${ readTraitMetaDataImpl[T] }

  private def readCaseClassMetaDataImpl[T: Type](using Quotes): Expr[CaseClassMetaData] =
    AnnotationReader().readCaseClassMetaData[T]

  private def readTraitMetaDataImpl[T: Type](using Quotes): Expr[TraitMetaData] =
    AnnotationReader().readTraitMetaData[T]

  private def addField(jObject: JObject, field: Field, jValue: JValue): JValue =
    jValue match
      case o: JObject =>
        if (field.embedded) JObject(jObject.obj ++ o.obj)
        else JObject(jObject.obj :+ (field.fieldName -> o))
      case other => JObject(jObject.obj :+ (field.fieldName -> other))

  private object Derivation:

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): JSON[A] =
      inline m match
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): JSON[A] =
      new JSON[A]:
        private val traitMetaData: TraitMetaData = readTraitMetaData[A]
        private val typeHintMap: Map[String, String] = traitMetaData.subtypes.collect {
          case (name, classMeta) if classMeta.typeHint.isDefined =>
            name -> classMeta.typeHint.get
        }
        private val reverseTypeHintMap: Map[String, String] = typeHintMap.map((on, n) => (n, on))
        private val jsons: Seq[JSON[Any]] = summonFormatters[mirrorOfSum.MirroredElemTypes]
        private val names: Seq[String] =
          constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
            .asInstanceOf[Vector[String]]
        private val jsonsByNames: Map[String, JSON[Any]] = names.zip(jsons).toMap

        override def read(jValue: JValue): JValidation[A] =
          jValue match
            case jObject: JObject =>
              val typeName = (jObject \ traitMetaData.typeDiscriminator).as[String]
              val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
              val parsed = jsonsByNames(originalTypeName).read(jObject)
              parsed.map(_.asInstanceOf[A])
            case x =>
              Validated.invalidNel(
                JSONParseError(s"JSON object expected. >>> trait >>> $jValue >>> ${traitMetaData}"))

        override def write(value: A): JValue =
          // we never get a trait here, only classes, it's safe to assume Product
          val originalTypeName = value.asInstanceOf[Product].productPrefix
          val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
          val json = jsonsByNames(originalTypeName).write(value)
          json ++ JObject(traitMetaData.typeDiscriminator -> JString(typeName))

    end deriveTrait

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): JSON[A] =
      new JSON[A]:
        private val caseClassMetaData: CaseClassMetaData = readCaseClassMetaData[A]
        private val jsons: Vector[JSON[Any]] = summonFormatters[mirrorOfProduct.MirroredElemTypes]
        private val fieldsAndJsons: Vector[(Field, JSON[Any])] = caseClassMetaData.fields.zip(jsons)

        private val fieldNames: Vector[String] = fieldsAndJsons.flatMap { (field, json) =>
          if (field.embedded) json.fields.toVector :+ field.name
          else Vector(field.name)
        }

        override val fields: Set[String] = fieldNames.toSet

        override def write(value: A): JValue =
          val caseClassFields = value.asInstanceOf[Product].productIterator
          jsons
            .zip(caseClassFields)
            .zip(caseClassMetaData.fields)
            .foldLeft[JValue](JObject()) { case (jObject, ((json, fieldValue), field)) =>
              addField(jObject.asInstanceOf[JObject], field, json.write(fieldValue))
            }

        override def read(jValue: JValue): JValidation[A] =
          jValue match
            case jObject: JObject =>
              for {
                fieldsAsAList <- fieldsAndJsons
                  .map((field, format) => readField(field, format, jObject))
                  .sequence
                fieldsAsTuple = Tuple.fromArray(fieldsAsAList.toArray)

              } yield mirrorOfProduct.fromTuple(
                fieldsAsTuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

            case x =>
              Validated.invalidNel(JSONParseError(s"JSON object expected. ${x}"))

        private def readField(field: Field, json: JSON[Any], jObject: JObject): JValidation[Any] =
          if (field.embedded) json.read(jObject)
          else io.sphere.json.field(field.fieldName, field.defaultArgument)(jObject)(json)

    end deriveCaseClass

    inline private def summonFormatters[T <: Tuple]: Vector[JSON[Any]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[JSON[t]]
            .asInstanceOf[JSON[Any]] +: summonFormatters[ts]
