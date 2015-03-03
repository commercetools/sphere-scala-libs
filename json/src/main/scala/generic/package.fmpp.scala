package io.sphere.json

import scalaz.{ ValidationNel, Success, NonEmptyList }
import scalaz.syntax.std.option._
import scalaz.syntax.applicative._

import scala.annotation.meta.getter
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.{ ClassTag, classTag }
import scala.tools.scalap.scalax.rules.scalasig._
import scala.util.control.NonFatal

import io.sphere.util.{ Reflect, Memoizer }

import org.json4s.JsonAST._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

/** The generic package provides generic functions for deriving JSON instances via
  * some runtime & compile-time reflection. */
package object generic {
  // Type aliases for more convenient use of the annotations in Scala code
  type JSONEmbedded = io.sphere.json.annotations.JSONEmbedded @getter
  type JSONKey = io.sphere.json.annotations.JSONKey @getter
  type JSONIgnore = io.sphere.json.annotations.JSONIgnore @getter
  type JSONTypeHint = io.sphere.json.annotations.JSONTypeHint
  type JSONTypeHintField = io.sphere.json.annotations.JSONTypeHintField

  type JSONParseResult[A] = ValidationNel[JSONError, A]

  def deriveJSON[A]: JSON[A] = macro JSONMacros.deriveJSON_impl[A]

  /** Creates a JSON instance for an Enumeration type that encodes the `toString`
    * representations of the enumeration values. */
  def jsonEnum(e: Enumeration): JSON[e.Value] = new JSON[e.Value] {
    def write(a: e.Value): JValue = JString(a.toString)
    def read(jval: JValue): ValidationNel[JSONError, e.Value] = jval match {
      case JString(s) => e.values.find(_.toString == s).toSuccess(
        NonEmptyList(JSONParseError("Invalid enum value: '%s'. Expected one of: %s".format(s, e.values.mkString("','"))))
      )
      case _ => jsonParseError("JSON String expected.")
    }
  }

  /** Creates a JSON instance for a singleton object that encodes only the type value
    * as a JSON string. */
  def jsonSingleton[T](singleton: T): JSON[T] = {
    val clazz = singleton.getClass
    val typeValue = getJSONClass(clazz).typeHint.map(_.value).getOrElse(defaultTypeValue(clazz))
    new JSON[T] {
      def write(t: T): JValue = JString(typeValue)
      def read(j: JValue): ValidationNel[JSONError, T] = j match {
        case JString(`typeValue`) => Success(singleton)
        case _ => jsonParseError("JSON string '" + typeValue + "' expected.")
      }
    }
  }

  /** Creates a JSON instance for a product type of arity 0 (case objects) that are part of a sum type. */
  def jsonProduct0[T <: Product](singleton: T): JSON[T] = {
    val (typeField, typeValue) = getJSONClass(singleton.getClass).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(singleton.getClass))
    }
    new JSON[T] {
      def write(t: T): JValue = JObject(JField(typeField, JString(typeValue)) :: Nil)
      def read(j: JValue): ValidationNel[JSONError, T] = j match {
        case o: JObject => findTypeValue(o, typeField) match {
          case Some(t) => t match {
            case `typeValue` => Success(singleton)
            case _ => jsonParseError("Invalid type value '" + t + "' in '%s'".format(compact(render(o))))
          }
          case None => jsonParseError("Missing type field '" + typeField + "' in '%s'".format(compact(render(o))))
        }
        case _ => jsonParseError("JSON object expected.")
      }
    }
  }

  <#list 1..22 as i>
  <#assign typeParams><#list 1..i as j>A${j}<#if i !=j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} : FromJSON : ToJSON<#if i !=j>,</#if></#list></#assign>
  /** Creates a `JSON[T]` instance for a product type (case class) `T` of arity ${i}. */
  def jsonProduct[T <: Product: ClassTag, ${implTypeParams}](
    construct: <#list 1..i as j>A${j}<#if i !=j> => </#if></#list> => T
  ): JSON[T] = {
    val jsonClass = getJSONClass(classTag[T].runtimeClass)
    val fields = jsonClass.fields
    new JSON[T] {
      def write(r: T): JValue = {
        val buf = new ListBuffer[JField]
        if (jsonClass.typeHint.isDefined) writeTypeField(jsonClass, buf)
        <#list 1..i as j>
        writeField[A${j}](buf, fields(${j-1}), r.productElement(${j-1}).asInstanceOf[A${j}])
        </#list>
        JObject(buf.toList)
      }
      def read(jval: JValue): ValidationNel[JSONError, T] = jval match {
        case o: JObject =>
          <#if i!=1>
            <#list i..2 as j>
            (readField[A${j}](fields(${j-1}), o) <*>
            </#list>
          </#if>
            readField[A1](fields(0), o).map(construct)<#list 1..i as j><#if i!=j>)</#if></#list>
        case _ => jsonParseError("JSON object expected.")
      }
    }
  }
  </#list>

  /** Creates a `JSON[T]` instance for some supertype `T`. The instance acts as a type-switch
    * for the subtypes `A1` and `A2`, delegating to their respective JSON instances based
    * on a field that acts as a type hint. */
  def jsonTypeSwitch[T: ClassTag, A1 <: T: ClassTag: FromJSON: ToJSON, A2 <: T: ClassTag: FromJSON: ToJSON](selectors: List[TypeSelector[_]]): JSON[T] = {
    val inSelectors = typeSelector[A1] :: typeSelector[A2] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.serializer match {
      case container: TypeSelectorContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })

    val readMapBuilder = Map.newBuilder[String, TypeSelector[_]]
    val writeMapBuilder = Map.newBuilder[Class[_], TypeSelector[_]]

    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
      writeMapBuilder += (s.clazz -> s)
    }

    val readMap = readMapBuilder.result
    val writeMap = writeMapBuilder.result
    val clazz = classTag[T].runtimeClass

    val typeField = Option(clazz.getAnnotation(classOf[JSONTypeHintField])) match {
      case Some(a) => a.value
      case None => defaultTypeFieldName
    }

    new JSON[T] with TypeSelectorContainer {
      override def typeSelectors: List[TypeSelector[_]] = allSelectors

      def read(jval: JValue): ValidationNel[JSONError, T] = jval match {
        case o: JObject =>
          findTypeValue(o, typeField) match {
            case Some(t) => readMap.get(t) match {
              case Some(ts) => ts.read(o).asInstanceOf[ValidationNel[JSONError, T]]
              case None => jsonParseError("Invalid type value '" + t + "' in '%s'".format(compact(render(o))))
            }
            case None => jsonParseError("Missing type field '" + typeField + "' in '%s'".format(compact(render(o))))
          }
        case _ => jsonParseError("JSON object expected.")
      }

      def write(t: T): JValue = writeMap.get(t.getClass) match {
        case Some(ts) =>
          ts.write(t) match {
            case o @ JObject(obj) if obj.exists(_._1 == ts.typeField) => o
            case j: JObject => j ~ JField(ts.typeField, JString(ts.typeValue))
            case j => throw new IllegalStateException("The json is not an object but a " + j.getClass)
          }

        case None => throw new IllegalStateException("Can't find a serializer for a class " + t.getClass)
      }
    }
  }

  <#list 3..80 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : FromJSON : ToJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def jsonTypeSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelector[_]]): JSON[T] = jsonTypeSwitch[T, ${typeParams}](typeSelector[A${i}] :: selectors)
  </#list>

  trait TypeSelectorContainer {
    def typeSelectors: List[TypeSelector[_]]
  }

  final class TypeSelector[A: FromJSON: ToJSON] private[generic](val typeField: String, val typeValue: String, val clazz: Class[_]) {
    def read(o: JObject): ValidationNel[JSONError, A] = fromJValue[A](o)
    def write(a: Any): JValue = toJValue(a.asInstanceOf[A])
    def serializer = implicitly[ToJSON[A]]
  }

  private def typeSelector[A: ClassTag: FromJSON: ToJSON](): TypeSelector[_] = {
    val clazz = classTag[A].runtimeClass
    val (typeField, typeValue) = getJSONClass(clazz).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(clazz))
    }
    new TypeSelector[A](typeField, typeValue, clazz)
  }

  private def defaultTypeValue(clazz: Class[_]): String =
    clazz.getSimpleName.replace("$", "")

  private def findTypeValue(o: JObject, typeField: String): Option[String] =
    o.obj.find(_._1 == typeField).flatMap(_._2.extractOpt[String])

  /** Extractor for type hints. */
  class TypeHint(field: String) {
    def unapply(jobj: JObject): Option[String] = findTypeValue(jobj, field)
  }

  /** The default name of the JSON field used for type-hinting, taken from the JSONTypeHintField annotation. */
  val defaultTypeFieldName: String = classOf[JSONTypeHintField].getMethod("value").getDefaultValue.asInstanceOf[String]

  private val getJSONClass = new Memoizer[Class[_], JSONClassMeta](clazz => {
    def hintVal(h: JSONTypeHint): String =
      if (h.value.isEmpty) defaultTypeValue(clazz)
      else h.value

    log.trace("Initializing JSON metadata for %s".format(clazz.getName))

    val typeHintFieldAnnot = clazz.getAnnotation(classOf[JSONTypeHintField])
    val typeHintAnnot = clazz.getAnnotation(classOf[JSONTypeHint])
    val typeField = Option(typeHintFieldAnnot).map(_.value)
    val typeValue = Option(typeHintAnnot).map(hintVal)

    JSONClassMeta(
      typeHint = (typeField, typeValue) match {
        case (Some(field), Some(hint)) => Some(JSONClassMeta.TypeHint(field, hint))
        case (None       , Some(hint)) => Some(JSONClassMeta.TypeHint(defaultTypeFieldName, hint))
        case (Some(field), None)       => Some(JSONClassMeta.TypeHint(field, defaultTypeValue(clazz)))
        case (None       , None)       => None
      },
      fields = getJSONFields(clazz)
    )
  })

  private case class JSONClassMeta(typeHint: Option[JSONClassMeta.TypeHint], fields: IndexedSeq[JSONFieldMeta])
  private object JSONClassMeta {
    case class TypeHint(field: String, value: String)
  }
  private case class JSONFieldMeta(
    name: String,
    default: Option[Any] = None,
    embedded: Boolean = false,
    ignored: Boolean = false)

  private def getJSONFields(clazz: Class[_]): IndexedSeq[JSONFieldMeta] = {
    Reflect.getCaseClassMeta(clazz).fields.map { fm =>
      val m = clazz.getDeclaredMethod(fm.name)
      val name = Option(m.getAnnotation(classOf[JSONKey])).map(_.value).getOrElse(fm.name)
      val embedded = m.isAnnotationPresent(classOf[JSONEmbedded])
      val ignored = m.isAnnotationPresent(classOf[JSONIgnore])
      if (ignored && !fm.default.isDefined) {
        // programmer error
        throw new JSONException("Ignored JSON field '%s' must have a default value.".format(fm.name))
      }
      JSONFieldMeta(fm.name, fm.default, embedded, ignored)
    }
  }

  private def writeField[A: ToJSON](buf: ListBuffer[JField], field: JSONFieldMeta, e: A) {
    if (!field.ignored) {
      if (field.embedded)
        buf ++= (toJValue(e) match {
          case o: JObject => o.obj
          case _ => Nil
        })
      else
        buf += JField(field.name, toJValue(e))
    }
  }

  private def writeTypeField(jClass: JSONClassMeta, buf: ListBuffer[JField]): Unit =
    jClass.typeHint foreach { th =>
      buf += JField(th.field, JString(th.value))
    }

  private def readField[A: FromJSON](f: JSONFieldMeta, o: JObject): JSONParseResult[A] = {
    val default = f.default.asInstanceOf[Option[A]]
    if (f.ignored) default.map(Success(_))/*.orElse(jsonr.default)*/.getOrElse {
      // programmer error
      throw new JSONException("Missing default for ignored field.")
    }
    else if (f.embedded) fromJValue[A](o)
    else field[A](f.name, default)(o)
  }
}
