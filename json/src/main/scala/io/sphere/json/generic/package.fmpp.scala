package io.sphere.json


import cats.data.Validated.Valid
import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.option._

import scala.annotation.meta.getter
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.{ClassTag, classTag}
import io.sphere.util.{Memoizer, Reflect}
import org.json4s.JsonAST._
import org.json4s.jackson.compactJson
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

  type JSONParseResult[A] = ValidatedNel[JSONError, A]

  def deriveJSON[A]: JSON[A] = macro JSONMacros.deriveJSON_impl[A]
  def deriveSingletonJSON[A]: JSON[A] = macro JSONMacros.deriveSingletonJSON_impl[A]

  private def JSONofToAndFrom[A](toJSON: ToJSON[A], fromJSON: FromJSON[A]): JSON[A] = {
    new JSON[A] {
      def write(a: A): JValue = toJSON.write(a)
      def read(jval: JValue): ValidatedNel[JSONError, A] = fromJSON.read(jval)
      override def fields: Set[String] = fromJSON.fields
    }
  }

  /** Creates a ToJSON instance for an Enumeration type that encodes the `toString`
    * representations of the enumeration values. */
  def toJsonEnum(e: Enumeration): ToJSON[e.Value] = new ToJSON[e.Value] {
    def write(a: e.Value): JValue = JString(a.toString)
  }

  /** Creates a FromJSON instance for an Enumeration type that encodes the `toString`
    * representations of the enumeration values. */
  def fromJsonEnum(e: Enumeration): FromJSON[e.Value] = new FromJSON[e.Value] {
    def read(jval: JValue): ValidatedNel[JSONError, e.Value] = jval match {
      case JString(s) => e.values.find(_.toString == s).toValidNel(
        JSONParseError("Invalid enum value: '%s'. Expected one of: %s".format(s, e.values.mkString("','")))
      )
      case _ => jsonParseError("JSON String expected.")
    }
  }

  /** Creates a JSON instance for an Enumeration type that encodes the `toString`
    * representations of the enumeration values. */
  def jsonEnum(e: Enumeration): JSON[e.Value] = JSONofToAndFrom(toJsonEnum(e), fromJsonEnum(e))

  private def jsonSingletonTypeValue[T](singleton: T): String = {
    val clazz = singleton.getClass
    getJSONClass(clazz).typeHint.map(_.value).getOrElse(defaultTypeValue(clazz))
  }

  /** Creates a ToJSON instance for a singleton object that encodes only the type value
    * as a JSON string. */
  def toJsonSingleton[T](singleton: T): ToJSON[T] = {
    val typeValue = jsonSingletonTypeValue(singleton)
    new ToJSON[T] {
      def write(t: T): JValue = JString(typeValue)
    }
  }

  /** Creates a FromJSON instance for a singleton object that encodes only the type value
    * as a JSON string. */
  def fromJsonSingleton[T](singleton: T): FromJSON[T] = {
    val typeValue = jsonSingletonTypeValue(singleton)
    new FromJSON[T] {
      def read(j: JValue): ValidatedNel[JSONError, T] = j match {
        case JString(`typeValue`) => Valid(singleton)
        case _ => jsonParseError("JSON string '" + typeValue + "' expected.")
      }
    }
  }

  /** Creates a JSON instance for a singleton object that encodes only the type value
    * as a JSON string. */
  def jsonSingleton[T](singleton: T): JSON[T] = JSONofToAndFrom(toJsonSingleton(singleton), fromJsonSingleton(singleton))

  private def jsonProduct0Type[T <: Product](singleton: T): (String, String) = {
    getJSONClass(singleton.getClass).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(singleton.getClass))
    }
  }

  /** Creates a ToJSON instance for a product type of arity 0 (case objects) that are part of a sum type. */
  def toJsonProduct0[T <: Product](singleton: T): ToJSON[T] = {
    val (typeField, typeValue) = jsonProduct0Type(singleton)
    new ToJSON[T] {
      def write(t: T): JValue = JObject(JField(typeField, JString(typeValue)) :: Nil)
    }
  }

  /** Creates a FromJSON instance for a product type of arity 0 (case objects) that are part of a sum type. */
  def fromJsonProduct0[T <: Product](singleton: T): FromJSON[T] = {
    val (typeField, typeValue) = jsonProduct0Type(singleton)
    new FromJSON[T] {
      def read(j: JValue): ValidatedNel[JSONError, T] = j match {
        case o: JObject => findTypeValue(o, typeField) match {
          case Some(t) => t match {
            case `typeValue` => Valid(singleton)
            case _ => jsonParseError("Invalid type value '" + t + "' in '%s'".format(compactJson(o)))
          }
          case None => jsonParseError("Missing type field '" + typeField + "' in '%s'".format(compactJson(o)))
        }
        case _ => jsonParseError("JSON object expected.")
      }
    }
  }

  /** Creates a JSON instance for a product type of arity 0 (case objects) that are part of a sum type. */
  def jsonProduct0[T <: Product](singleton: T): JSON[T] = JSONofToAndFrom(toJsonProduct0(singleton), fromJsonProduct0(singleton))

  <#list 1..22 as i>
  <#assign typeParams><#list 1..i as j>A${j}<#if i !=j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} : ToJSON<#if i !=j>,</#if></#list></#assign>
  /** Creates a `ToJSON[T]` instance for a product type (case class) `T` of arity ${i}. */
  def toJsonProduct[T <: Product: ClassTag, ${implTypeParams}](
    construct: (<#list 1..i as j>A${j}<#if i !=j>, </#if></#list>) => T
  ): ToJSON[T] = {
    val jsonClass = getJSONClass(classTag[T].runtimeClass)
    val _fields = jsonClass.fields
    new ToJSON[T] {
      def write(r: T): JValue = {
        val buf = new ListBuffer[JField]
        if (jsonClass.typeHint.isDefined) writeTypeField(jsonClass, buf)
        <#list 1..i as j>
          writeField[A${j}](buf, _fields(${j-1}), r.productElement(${j-1}).asInstanceOf[A${j}])
        </#list>
        JObject(buf.toList)
      }
    }
  }
  </#list>

  <#list 1..22 as i>
  <#assign typeParams><#list 1..i as j>A${j}<#if i !=j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} : FromJSON<#if i !=j>,</#if></#list></#assign>
  /** Creates a `FromJSON[T]` instance for a product type (case class) `T` of arity ${i}. */
  def fromJsonProduct[T <: Product: ClassTag, ${implTypeParams}](
    construct: (<#list 1..i as j>A${j}<#if i !=j>, </#if></#list>) => T
  ): FromJSON[T] = {
    val jsonClass = getJSONClass(classTag[T].runtimeClass)
    val _fields = jsonClass.fields
    new FromJSON[T] {
      def read(jval: JValue): ValidatedNel[JSONError, T] = jval match {
        case o: JObject =>
          (readField[A1](_fields.head, o)<#if i!=1>
          <#list 2..i as j>
      , readField[A${j}](_fields(${j-1}), o)
      </#list>
      </#if>
      ).map<#if i!=1>N</#if>(construct)
        case _ => jsonParseError("JSON object expected.")
      }
      override val fields = _fields.map(_.name).toSet
    }
  }
  </#list>

  <#list 1..22 as i>
  <#assign typeParams><#list 1..i as j>A${j}<#if i !=j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} : FromJSON : ToJSON<#if i !=j>,</#if></#list></#assign>
  /** Creates a `JSON[T]` instance for a product type (case class) `T` of arity ${i}. */
  def jsonProduct[T <: Product: ClassTag, ${implTypeParams}](
    construct: (<#list 1..i as j>A${j}<#if i !=j>, </#if></#list>) => T
  ): JSON[T] = JSONofToAndFrom(toJsonProduct(construct), fromJsonProduct(construct))
  </#list>

  /**
    * Creates a `ToJSON[T]` instance for some supertype `T`. The instance acts as a type-switch between implementation which should be
    * a singleton case objects.
    *
    * This can be used as an alternative to an enum.
    */
  def toJsonSingletonEnumSwitch[T: ClassTag, A <: T : ClassTag : ToJSON](selectors: List[TypeSelectorToJSON[_]]): ToJSON[T] with TypeSelectorToJSONContainer = {
    val inSelectors: List[TypeSelectorToJSON[_]] = typeSelectorToJSON[A] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.serializer match {
      case container: TypeSelectorToJSONContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })
    val writeMapBuilder = Map.newBuilder[Class[_], TypeSelectorToJSON[_]]
    allSelectors.foreach { s =>
      writeMapBuilder += (s.clazz -> s)
    }
    val writeMap = writeMapBuilder.result

    new ToJSON[T] with TypeSelectorToJSONContainer {
      override def typeSelectors = allSelectors

      def write(t: T): JValue = writeMap.get(t.getClass) match {
        case Some(ts) =>
          ts.write(t) match {
            case s: JString => s
            case j => throw new IllegalStateException("The json is not a string, but a " + j.getClass)
          }

        case None => throw new IllegalStateException("Can't find a serializer for a class " + t.getClass)
      }
    }
  }

  /**
    * Creates a `FromJSON[T]` instance for some supertype `T`. The instance acts as a type-switch between implementation which should be
    * a singleton case objects.
    *
    * This can be used as an alternative to an enum.
    */
  def fromJsonSingletonEnumSwitch[T: ClassTag, A <: T : ClassTag : FromJSON](selectors: List[TypeSelectorFromJSON[_]]): FromJSON[T] with TypeSelectorFromJSONContainer = {
    val readMapBuilder = Map.newBuilder[String, TypeSelectorFromJSON[_]]
    val inSelectors: List[TypeSelectorFromJSON[_]] = typeSelectorFromJSON[A] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.jsonr match {
      case container: TypeSelectorFromJSONContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })
    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
    }
    val readMap = readMapBuilder.result

    new FromJSON[T] with TypeSelectorFromJSONContainer {
      override def typeSelectors = allSelectors

      def read(jval: JValue): ValidatedNel[JSONError, T] = jval match {
        case s @ JString(typeName) =>
          readMap.get(typeName) match {
            case Some(ts) => ts.read(s).asInstanceOf[ValidatedNel[JSONError, T]]
            case None => jsonParseError("Invalid value '" + typeName + "'.")
          }

        case _ => jsonParseError("JSON string expected.")
      }
    }
  }

  /**
    * Creates a `JSON[T]` instance for some supertype `T`. The instance acts as a type-switch between implementation which should be
    * a singleton case objects.
    *
    * This can be used as an alternative to an enum.
    */
  def jsonSingletonEnumSwitch[T: ClassTag, A <: T : ClassTag : FromJSON : ToJSON](selectors: List[TypeSelector[_]]): JSON[T] with TypeSelectorContainer = {
    val inSelectors = typeSelector[A] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.serializer match {
      case container: TypeSelectorContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })

    val toJSON = toJsonSingletonEnumSwitch[T, A](selectors)
    val fromJSON = fromJsonSingletonEnumSwitch[T, A](selectors)

    new JSON[T] with TypeSelectorContainer {
      override def typeSelectors: List[TypeSelector[_]] = allSelectors

      def read(jval: JValue): ValidatedNel[JSONError, T] = fromJSON.read(jval)

      def write(t: T): JValue = toJSON.write(t)
    }
  }

  <#list 2..20 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : ToJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def toJsonSingletonEnumSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelectorToJSON[_]]): ToJSON[T] with TypeSelectorToJSONContainer = toJsonSingletonEnumSwitch[T, ${typeParams}](typeSelectorToJSON[A${i}] :: selectors)
  </#list>

  <#list 2..20 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : FromJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def fromJsonSingletonEnumSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelectorFromJSON[_]]): FromJSON[T] with TypeSelectorFromJSONContainer = fromJsonSingletonEnumSwitch[T, ${typeParams}](typeSelectorFromJSON[A${i}] :: selectors)
  </#list>

  <#list 2..20 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : FromJSON : ToJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def jsonSingletonEnumSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelector[_]]): JSON[T] with TypeSelectorContainer = jsonSingletonEnumSwitch[T, ${typeParams}](typeSelector[A${i}] :: selectors)
  </#list>

  /** Creates a `ToJSON[T]` instance for some supertype `T`. The instance acts as a type-switch
    * for the subtypes `A1` and `A2`, delegating to their respective JSON instances based
    * on a field that acts as a type hint. */
  def toJsonTypeSwitch[T: ClassTag, A1 <: T: ClassTag: ToJSON, A2 <: T: ClassTag: ToJSON](selectors: List[TypeSelectorToJSON[_]]): ToJSON[T] with TypeSelectorToJSONContainer = {
    val inSelectors = typeSelectorToJSON[A1] :: typeSelectorToJSON[A2] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.serializer match {
      case container: TypeSelectorToJSONContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })

    val writeMapBuilder = Map.newBuilder[Class[_], TypeSelectorToJSON[_]]

    allSelectors.foreach { s =>
      writeMapBuilder += (s.clazz -> s)
    }

    val writeMap = writeMapBuilder.result

    new ToJSON[T] with TypeSelectorToJSONContainer {
      override def typeSelectors: List[TypeSelectorToJSON[_]] = allSelectors

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

  /** Creates a `FromJSON[T]` instance for some supertype `T`. The instance acts as a type-switch
    * for the subtypes `A1` and `A2`, delegating to their respective JSON instances based
    * on a field that acts as a type hint. */
  def fromJsonTypeSwitch[T: ClassTag, A1 <: T: ClassTag: FromJSON, A2 <: T: ClassTag: FromJSON](selectors: List[TypeSelectorFromJSON[_]]): FromJSON[T] with TypeSelectorFromJSONContainer = {
    val inSelectors = typeSelectorFromJSON[A1] :: typeSelectorFromJSON[A2] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.jsonr match {
      case container: TypeSelectorFromJSONContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })

    val readMapBuilder = Map.newBuilder[String, TypeSelectorFromJSON[_]]

    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
    }

    val readMap = readMapBuilder.result
    val clazz = classTag[T].runtimeClass

    val typeField = Option(clazz.getAnnotation(classOf[JSONTypeHintField])) match {
      case Some(a) => a.value
      case None => defaultTypeFieldName
    }

    new FromJSON[T] with TypeSelectorFromJSONContainer {
      override def typeSelectors: List[TypeSelectorFromJSON[_]] = allSelectors

      def read(jval: JValue): ValidatedNel[JSONError, T] = jval match {
        case o: JObject =>
          findTypeValue(o, typeField) match {
            case Some(t) => readMap.get(t) match {
              case Some(ts) => ts.read(o).asInstanceOf[ValidatedNel[JSONError, T]]
              case None => jsonParseError("Invalid type value '" + t + "' in '%s'".format(compactJson(o)))
            }
            case None => jsonParseError("Missing type field '" + typeField + "' in '%s'".format(compactJson(o)))
          }
        case _ => jsonParseError("JSON object expected.")
      }
    }
  }

  /** Creates a `JSON[T]` instance for some supertype `T`. The instance acts as a type-switch
    * for the subtypes `A1` and `A2`, delegating to their respective JSON instances based
    * on a field that acts as a type hint. */
  def jsonTypeSwitch[T: ClassTag, A1 <: T: ClassTag: FromJSON: ToJSON, A2 <: T: ClassTag: FromJSON: ToJSON](selectors: List[TypeSelector[_]]): JSON[T] with TypeSelectorContainer = {
    val inSelectors = typeSelector[A1] :: typeSelector[A2] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.serializer match {
      case container: TypeSelectorContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })

    val toJSON = toJsonTypeSwitch[T, A1, A2](selectors)
    val fromJSON = fromJsonTypeSwitch[T, A1, A2](selectors)

    new JSON[T] with TypeSelectorContainer {
      override def typeSelectors: List[TypeSelector[_]] = allSelectors

      def read(jval: JValue): ValidatedNel[JSONError, T] = fromJSON.read(jval)

      def write(t: T): JValue = toJSON.write(t)
    }
  }

  <#list 3..84 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : ToJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def toJsonTypeSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelectorToJSON[_]]): ToJSON[T] with TypeSelectorToJSONContainer = toJsonTypeSwitch[T, ${typeParams}](typeSelectorToJSON[A${i}] :: selectors)
  </#list>

  <#list 3..84 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : FromJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def fromJsonTypeSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelectorFromJSON[_]]): FromJSON[T] with TypeSelectorFromJSONContainer = fromJsonTypeSwitch[T, ${typeParams}](typeSelectorFromJSON[A${i}] :: selectors)
  </#list>

  <#list 3..84 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : FromJSON : ToJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def jsonTypeSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelector[_]]): JSON[T] with TypeSelectorContainer = jsonTypeSwitch[T, ${typeParams}](typeSelector[A${i}] :: selectors)
  </#list>

  trait TypeSelectorBase {
    def typeField: String
    def typeValue: String
    def clazz: Class[_]
  }

  trait TypeSelectorToJSONContainer {
    def typeSelectors: List[TypeSelectorToJSON[_]]
  }

  trait TypeSelectorToJSON[A] extends TypeSelectorBase {
    def write(a: Any): JValue
    def serializer: ToJSON[A]
  }

  final class TypeSelectorToJSONImpl[A] private[generic](val typeField: String, val typeValue: String, val clazz: Class[_])(implicit val serializer: ToJSON[A]) extends TypeSelectorToJSON[A] {
    def write(a: Any): JValue = toJValue(a.asInstanceOf[A])
  }

  private def typeSelectorToJSON[A: ClassTag: ToJSON](): TypeSelectorToJSON[_] = {
    val clazz = classTag[A].runtimeClass
    val (typeField, typeValue) = getJSONClass(clazz).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(clazz))
    }
    new TypeSelectorToJSONImpl[A](typeField, typeValue, clazz)
  }

  trait TypeSelectorFromJSONContainer {
    def typeSelectors: List[TypeSelectorFromJSON[_]]
  }

  trait TypeSelectorFromJSON[A] extends TypeSelectorBase {
    def read(o: JValue): ValidatedNel[JSONError, A]
    def jsonr: FromJSON[A]
  }

  final class TypeSelectorFromJSONImpl[A] private[generic](val typeField: String, val typeValue: String, val clazz: Class[_])(implicit val jsonr: FromJSON[A]) extends TypeSelectorFromJSON[A] {
    def read(o: JValue): ValidatedNel[JSONError, A] = fromJValue[A](o)
  }

  private def typeSelectorFromJSON[A: ClassTag: FromJSON](): TypeSelectorFromJSON[_] = {
    val clazz = classTag[A].runtimeClass
    val (typeField, typeValue) = getJSONClass(clazz).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(clazz))
    }
    new TypeSelectorFromJSONImpl[A](typeField, typeValue, clazz)
  }

  trait TypeSelectorContainer extends TypeSelectorFromJSONContainer with TypeSelectorToJSONContainer {
    def typeSelectors: List[TypeSelector[_]]
  }

  final class TypeSelector[A] private[generic](val typeField: String, val typeValue: String, val clazz: Class[_])(implicit val jsonr: FromJSON[A], val serializer: ToJSON[A]) extends TypeSelectorFromJSON[A] with TypeSelectorToJSON[A] {
    def read(o: JValue): ValidatedNel[JSONError, A] = fromJValue[A](o)
    def write(a: Any): JValue = toJValue(a.asInstanceOf[A])
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
      if (ignored && fm.default.isEmpty) {
        // programmer error
        throw new JSONException("Ignored JSON field '%s' must have a default value.".format(fm.name))
      }
      JSONFieldMeta(name, fm.default, embedded, ignored)
    }
  }

  private def writeField[A: ToJSON](buf: ListBuffer[JField], field: JSONFieldMeta, e: A) {
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

  private def writeTypeField(jClass: JSONClassMeta, buf: ListBuffer[JField]): Unit =
    jClass.typeHint foreach { th =>
      buf += JField(th.field, JString(th.value))
    }

  private def readField[A: FromJSON](f: JSONFieldMeta, o: JObject): JSONParseResult[A] = {
    val default = f.default.asInstanceOf[Option[A]]
    if (f.ignored) default.map(Valid(_))/*.orElse(jsonr.default)*/.getOrElse {
      // programmer error
      throw new JSONException("Missing default for ignored field.")
    }
    else if (f.embedded) fromJValue[A](o)
    else field[A](f.name, default)(o)
  }
}
