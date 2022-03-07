package io.sphere.json.generic

import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import io.sphere.json.{
  FromJSON,
  JSON,
  JSONError,
  JSONException,
  JSONParseError,
  JValidation,
  ToJSON,
  field,
  fromJValue,
  jsonParseError,
  toJValue
}
import io.sphere.util.{Logging, Memoizer, Reflect}
import magnolia._
import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JField, JObject, JString, JValue}
import org.json4s.jackson.compactJson
import org.json4s.JsonDSL._
import scala.language.experimental.macros
import scala.collection.mutable.ListBuffer

package object mgn extends Logging {

  type Typeclass[T] = JSON[T]

  def deriveJSON[A]: JSON[A] = macro Magnolia.gen[A]
  // def deriveSingletonJSON[A]: JSON[A] = macro JSONMacros.deriveSingletonJSON_impl[A]

  type JSONParseResult[A] = ValidatedNel[JSONError, A]

  def combine[T](caseClass: CaseClass[JSON, T]): JSON[T] = new JSON[T] {
    private val jsonClass = getJSONClassMeta(caseClass)
    private val _fields = jsonClass.fields

    override def read(jval: JValue): JValidation[T] = jval match {
      case o: JObject =>
        val instance: Either[NonEmptyList[JSONParseError], T] = caseClass
          .constructEither { param =>
            val field = fieldMeta(param)
            readField(field, o)(param.typeclass).toEither
          }
          .leftMap(_ => NonEmptyList.one(JSONParseError("JSON object expected.")))

        Validated.fromEither(instance)
      case _ => jsonParseError("JSON object expected.")
    }

    override def write(value: T): JValue = {
      val buf = new ListBuffer[JField]
      val hintField: JField = jsonClass.typeHint match {
        case Some(th) => JField(th.field, JString(th.value))
        case None => null
      }
      if (hintField != null) buf += hintField
      caseClass.parameters.foreach { param =>
        writeField(buf, fieldMeta(param), param.dereference(value))(param.typeclass)
      }
      JObject(buf.toList)
    }

    // TODO extract into common magnolia module
    override val fields: Set[String] = calculateFields()
    private def calculateFields(): Set[String] = {
      val builder = Set.newBuilder[String]
      var i = 0
      caseClass.parameters.foreach { p =>
        val f = _fields(i)
        if (!f.ignored) {
          if (f.embedded)
            builder ++= p.typeclass.fields
          else
            builder += f.name
        }
        i += 1
      }
      builder.result()
    }
  }
  def dispatch[T](sealedTrait: SealedTrait[JSON, T]): JSON[T] = {
    val allSelectors = sealedTrait.subtypes.map { subType =>
      typeSelector(subType)
    }
    val readMapBuilder = Map.newBuilder[String, TypeSelector[_]]
    val writeMapBuilder = Map.newBuilder[TypeName, TypeSelector[_]]
    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
      writeMapBuilder += (s.subType.typeName -> s)
    }
    val readMap = readMapBuilder.result()
    val writeMap = writeMapBuilder.result()
    val typeField = sealedTrait.annotations
      .collectFirst { case a: JSONTypeHintField =>
        a.value
      }
      .getOrElse(defaultTypeFieldName)

    new JSON[T] {
      override def write(value: T): JValue = sealedTrait.dispatch(value) { subtype =>
        writeMap.get(subtype.typeName) match {
          case Some(ts) =>
            subtype.typeclass.write(subtype.cast(value)) match {
              case o @ JObject(obj) if obj.exists(_._1 == ts.typeField) => o
              case j: JObject => j ~ JField(ts.typeField, JString(ts.typeValue))
              case j =>
                throw new IllegalStateException("The json is not an object but a " + j.getClass)
            }
          case None =>
            throw new IllegalStateException("Can't find a serializer for a class " + value.getClass)
        }
      }

      override def read(jval: JValue): JValidation[T] = jval match {
        case o: JObject =>
          findTypeValue(o, typeField) match {
            case Some(t) =>
              readMap.get(t) match {
                case Some(ts) => ts.subType.typeclass.read(o).asInstanceOf[JValidation[T]]
                case None =>
                  jsonParseError("Invalid type value '" + t + "' in '%s'".format(compactJson(o)))
              }
            case None =>
              jsonParseError(
                "Missing type field '" + typeField + "' in '%s'".format(compactJson(o)))
          }
        case _ => jsonParseError("JSON object expected.")
      }
    }
  }

  private def findTypeValue(o: JObject, typeField: String): Option[String] = {
    implicit val formats = DefaultFormats
    o.obj.find(_._1 == typeField).flatMap(_._2.extractOpt[String])
  }

  private case class TypeSelector[A](
      val typeField: String,
      val typeValue: String,
      subType: Subtype[JSON, A])

  private def typeSelector[A](subType: Subtype[JSON, A]): TypeSelector[A] = {
    val (typeField, typeValue) = getJSONClassMetaFromSubType(subType).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(subType.typeName))
    }
    new TypeSelector[A](typeField, typeValue, subType)
  }

  private def getJSONFields(caseClass: CaseClass[JSON, _]): IndexedSeq[JSONFieldMeta] =
    caseClass.parameters.map(fieldMeta).toIndexedSeq

  private def fieldMeta(p: Param[JSON, _]): JSONFieldMeta = {
    val annotations = p.annotations
    val name = annotations
      .collectFirst { case h: JSONKey =>
        h
      }
      .fold(p.label)(_.value)
    val embedded = annotations.exists {
      case _: JSONEmbedded => true
      case _ => false
    }
    val ignored = annotations.exists {
      case _: JSONIgnore => true
      case _ => false
    }
    if (ignored && p.default.isEmpty) {
      throw new JSONException("Ignored JSON field '%s' must have a default value.".format(p.label))
    }
    JSONFieldMeta(name, p.default, embedded, ignored)
  }

  // TODO might not be needed
  private def getJSONClassMeta = new Memoizer[CaseClass[JSON, _], JSONClassMeta](caseClass => {
    log.trace("Initializing JSON metadata for %s".format(caseClass.typeName.full))
    val annotations = caseClass.annotations
    val typeHintFieldAnnotation = annotations.collectFirst { case h: JSONTypeHintField =>
      h
    }
    val typeHintAnnotation: Option[JSONTypeHint] = annotations.collectFirst {
      case h: JSONTypeHint =>
        h
    }
    val typeField = typeHintFieldAnnotation.map(_.value)
    val typeValue = typeHintAnnotation.map(hintVal(caseClass.typeName))
    JSONClassMeta(
      typeHint = (typeField, typeValue) match {
        case (Some(field), Some(hint)) => Some(JSONClassMeta.TypeHint(field, hint))
        case (None, Some(hint)) => Some(JSONClassMeta.TypeHint(defaultTypeFieldName, hint))
        case (Some(field), None) =>
          Some(JSONClassMeta.TypeHint(field, defaultTypeValue(caseClass.typeName)))
        case (None, None) => None
      },
      fields = getJSONFields(caseClass)
    )
  })

  private val getJSONClassMetaFromSubType =
    new Memoizer[Subtype[JSON, _], JSONClassMeta](subType => {
      log.trace("Initializing Mongo metadata for %s".format(subType.typeName.full))

      val annotations = subType.annotations

      val typeHintFieldAnnot: Option[JSONTypeHintField] = annotations.collectFirst {
        case h: JSONTypeHintField => h
      }
      val typeHintAnnot: Option[JSONTypeHint] = annotations.collectFirst { case h: JSONTypeHint =>
        h
      }
      val typeField = typeHintFieldAnnot.map(_.value)
      val typeValue = typeHintAnnot.map(hintVal(subType.typeName))

      JSONClassMeta(
        typeHint = (typeField, typeValue) match {
          case (Some(field), Some(hint)) => Some(JSONClassMeta.TypeHint(field, hint))
          case (None, Some(hint)) => Some(JSONClassMeta.TypeHint(defaultTypeFieldName, hint))
          case (Some(field), None) =>
            Some(JSONClassMeta.TypeHint(field, defaultTypeValue(subType.typeName)))
          case (None, None) => None
        },
        fields = IndexedSeq[JSONFieldMeta]()
      )
    })

  private val defaultTypeFieldName: String = JSONTypeHintField.defaultValue

  private def defaultTypeValue(typeName: TypeName): String =
    typeName.short.replace("$", "")

  private def hintVal(typeName: TypeName)(h: JSONTypeHint): String =
    if (h.value.trim.isEmpty) defaultTypeValue(typeName)
    else h.value

  private case class JSONClassMeta(
      typeHint: Option[JSONClassMeta.TypeHint],
      fields: IndexedSeq[JSONFieldMeta])
  private object JSONClassMeta {
    case class TypeHint(field: String, value: String)
  }
  private case class JSONFieldMeta(
      name: String,
      default: Option[Any] = None,
      embedded: Boolean = false,
      ignored: Boolean = false)

  private def readField[A: FromJSON](f: JSONFieldMeta, o: JObject): JSONParseResult[A] = {
    def default = f.default.asInstanceOf[Option[A]]
    if (f.ignored) {
      default match {
        case Some(v) => Valid(v)
        case None =>
          // programmer error
          throw new JSONException("Missing default for ignored field.")
      }
    } else if (f.embedded) fromJValue[A](o)
    else field[A](f.name, default)(o)
  }

  private def writeField[A: ToJSON](buf: ListBuffer[JField], field: JSONFieldMeta, e: A): Unit =
    if (!field.ignored) {
      if (field.embedded)
        toJValue(e) match {
          case o: JObject => buf ++= o.obj
          case _ => // no update on buf
        }
      else
        buf += JField(field.name, toJValue(e))
    }

}
