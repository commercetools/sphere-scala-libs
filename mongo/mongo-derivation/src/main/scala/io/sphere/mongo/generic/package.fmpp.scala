package io.sphere.mongo

import scala.annotation.meta.getter
import scala.reflect.{ClassTag, classTag}
import scala.language.experimental.macros
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format._
import io.sphere.util.{Logging, Memoizer, Reflect}
import com.mongodb.BasicDBObject
import org.bson.BSONObject

/**
  * copy/paste from https://github.com/sphereio/sphere-scala-libs/blob/master/json/src/main/scala/generic/package.fmpp.scala
  * adapted to `MongoFormat`.
  */
package object generic extends Logging {

  type MongoEmbedded = io.sphere.mongo.generic.annotations.MongoEmbedded @getter
  type MongoKey = io.sphere.mongo.generic.annotations.MongoKey @getter
  type MongoIgnore = io.sphere.mongo.generic.annotations.MongoIgnore @getter
  type MongoTypeHint = io.sphere.mongo.generic.annotations.MongoTypeHint
  type MongoTypeHintField = io.sphere.mongo.generic.annotations.MongoTypeHintField
  type MongoProvidedFormatter = io.sphere.mongo.generic.annotations.MongoProvidedFormatter

  def deriveMongoFormat[A]: MongoFormat[A] = macro MongoFormatMacros.deriveMongoFormat_impl[A]

  /** The default name of the field used for type-hinting, taken from the MongoTypeHintField annotation. */
  val defaultTypeFieldName: String = classOf[MongoTypeHintField].getMethod("value").getDefaultValue.asInstanceOf[String]

  /**
    * Creates a MongoFormat instance for an Enumeration type that encodes the `toString`
    * representations of the enumeration values.
    */
  def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
    def toMongoValue(a: e.Value): Any = a.toString
    def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
  }

  private def mongoProduct0Type[T <: Product](singleton: T): (String, String) = {
    getMongoClassMeta(singleton.getClass).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(singleton.getClass))
    }
  }

  def mongoProduct0[T <: Product](singleton: T): MongoFormat[T] = {
    val (typeField, typeValue) = mongoProduct0Type(singleton)
    new MongoFormat[T] {
      override def toMongoValue(a: T): Any = {
        val dbo = new BasicDBObject()
        dbo.append(typeField, typeValue)
        dbo
      }
      override def fromMongoValue(any: Any): T = any match {
        case o: BSONObject => findTypeValue(o, typeField) match {
          case Some(t) if t == typeValue => singleton
          case Some(t) => sys.error("Invalid type value '" + t + "'. Excepted '%s'".format(typeValue))
          case None => sys.error("Missing type field.")
        }
        case _ => sys.error("DB object excepted.")
      }
    }
  }

  <#list 1..22 as i>
  <#assign typeParams><#list 1..i as j>A${j}<#if i !=j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j}: MongoFormat<#if i !=j>, </#if></#list></#assign>
  /** Creates a `MongoFormat[T]` instance for a product type (case class) `T` of arity ${i}. */
  def mongoProduct[T <: Product: ClassTag, ${implTypeParams}](
    construct: (<#list 1..i as j>A${j}<#if i !=j>, </#if></#list>) => T
  ): MongoFormat[T] = {
    val mongoClass = getMongoClassMeta(classTag[T].runtimeClass)
    val _fields = mongoClass.fields
    val _firstField = _fields.head
    val _withTypeHint = mongoClass.typeHint.isDefined
    new MongoFormat[T] {
      def toMongoValue(r: T): Any = {
        val dbo = new BasicDBObject
        if (_withTypeHint) {
          val th = mongoClass.typeHint.get
          dbo.put(th.field, th.value)
        }
        <#list 1..i as j>
          writeField[A${j}](dbo, _fields(${j-1}), r.productElement(${j-1}).asInstanceOf[A${j}])
        </#list>
        dbo
      }
      def fromMongoValue(any: Any): T = any match {
        case dbo: BSONObject =>
          construct(
            readField[A1](_firstField, dbo)<#if i!=1><#list 2..i as j>,
            readField[A${j}](_fields(${j-1}), dbo)</#list></#if>
          )
        case _ => sys.error("Deserialization failed. DBObject expected.")
      }
      override val fields: Set[String] = calculateFields()
      private def calculateFields(): Set[String] = {
        val builder = Set.newBuilder[String]
        <#list 1..i as j>
          val f${j} = _fields(${j-1})
          if (!f${j}.ignored) {
            if (f${j}.embedded)
              builder ++= MongoFormat[A${j}].fields
            else
              builder += f${j}.name
          }
        </#list>
        builder.result()
      }
    }
  }
  </#list>


  /** Derives a `MongoFormat[T]` instance for some supertype `T`. The instance acts as a type-switch
    * for the subtypes `A1` and `A2`, delegating to their respective MongoFormat instances based
    * on a field that acts as a type hint. */
  def mongoTypeSwitch[T: ClassTag, A1 <: T: ClassTag: MongoFormat, A2 <: T: ClassTag: MongoFormat](selectors: List[TypeSelector[_]]): MongoFormat[T] = {
    val allSelectors = typeSelector[A1]() :: typeSelector[A2]() :: selectors
    val readMapBuilder = Map.newBuilder[String, TypeSelector[_]]
    val writeMapBuilder = Map.newBuilder[Class[_], TypeSelector[_]]
    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
      writeMapBuilder += (s.clazz -> s)
    }
    val readMap = readMapBuilder.result()
    val writeMap = writeMapBuilder.result()
    val clazz = classTag[T].runtimeClass

    val fieldWithMongoTypeHintField = clazz.getAnnotation(classOf[MongoTypeHintField])
    val typeField = if (fieldWithMongoTypeHintField != null) fieldWithMongoTypeHintField.value() else defaultTypeFieldName

    new MongoFormat[T] {
      def fromMongoValue(any: Any): T = any match {
        case dbo: BSONObject =>
          findTypeValue(dbo, typeField) match {
            case Some(t) => readMap.get(t) match {
              case Some(r) => r.read(dbo).asInstanceOf[T]
              case None => sys.error("Invalid type value '" + t + "' in DBObject '%s'.".format(dbo))
            }
            case None => sys.error("Missing type field '" + typeField + "' in DBObject '%s'.".format(dbo))
          }
        case _ => sys.error("DBObject expected.")
      }
      def toMongoValue(t: T): Any = writeMap.get(t.getClass) match {
        case Some(w) => w.write(t) match {
          case dbo: BSONObject => findTypeValue(dbo, typeField) match {
            case Some(_) => dbo
            case None =>
              dbo.put(typeField, w.typeValue)
              dbo
          }
          case _ => throw new Exception("Excepted 'BSONObject'")
        }
        case None => new BasicDBObject(defaultTypeFieldName, defaultTypeValue(t.getClass))
      }
    }
  }

  // special case with only one sub-type
  def mongoTypeSwitch[T: ClassTag, A1 <: T: ClassTag: MongoFormat](selectors: List[TypeSelector[_]]): MongoFormat[T] =
    mongoTypeSwitch[T, A1, A1](selectors)

  <#list 3..80 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : MongoFormat : ClassTag<#if i !=j>,</#if></#list></#assign>
  def mongoTypeSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelector[_]]): MongoFormat[T] = mongoTypeSwitch[T, ${typeParams}](typeSelector[A${i}]() :: selectors)
  </#list>

  final class TypeSelector[A: MongoFormat](val typeValue: String, val clazz: Class[_]) {
    def read(any: Any): A = fromMongo[A](any)
    def write(a: Any): Any = toMongo[A](a.asInstanceOf[A])
  }

  private case class MongoClassMeta(typeHint: Option[MongoClassMeta.TypeHint], fields: IndexedSeq[MongoFieldMeta])
  private object MongoClassMeta {
    case class TypeHint(field: String, value: String)
  }
  private case class MongoFieldMeta(
    name: String,
    default: Option[Any] = None,
    embedded: Boolean = false,
    ignored: Boolean = false
  )

  private val getMongoClassMeta = new Memoizer[Class[_], MongoClassMeta](clazz => {
    def hintVal(h: MongoTypeHint): String =
      if (h.value.trim.isEmpty) defaultTypeValue(clazz)
      else h.value

    log.trace("Initializing Mongo metadata for %s".format(clazz.getName))

    val typeHintFieldAnnot = clazz.getAnnotation(classOf[MongoTypeHintField])
    val typeHintAnnot = clazz.getAnnotation(classOf[MongoTypeHint])
    val typeField = Option(typeHintFieldAnnot).map(_.value)
    val typeValue = Option(typeHintAnnot).map(hintVal)

    MongoClassMeta(
      typeHint = (typeField, typeValue) match {
        case (Some(field), Some(hint)) => Some(MongoClassMeta.TypeHint(field, hint))
        case (None       , Some(hint)) => Some(MongoClassMeta.TypeHint(defaultTypeFieldName, hint))
        case (Some(field), None)       => Some(MongoClassMeta.TypeHint(field, defaultTypeValue(clazz)))
        case (None       , None)       => None
      },
      fields = getMongoFieldMeta(clazz)
    )
  })

  private def getMongoFieldMeta(clazz: Class[_]): IndexedSeq[MongoFieldMeta] = {
    Reflect.getCaseClassMeta(clazz).fields.map { fm =>
      val m = clazz.getDeclaredMethod(fm.name)
      val fieldWithMongoKey = m.getAnnotation(classOf[MongoKey])
      val name = if (fieldWithMongoKey != null) fieldWithMongoKey.value else fm.name
      val embedded = m.isAnnotationPresent(classOf[MongoEmbedded])
      val ignored = m.isAnnotationPresent(classOf[MongoIgnore])
      if (ignored && fm.default.isEmpty) {
        throw new Exception("Ignored Mongo field '%s' must have a default value.".format(fm.name))
      }
      MongoFieldMeta(name, fm.default, embedded, ignored)
    }
  }

  private def writeField[A: MongoFormat](dbo: BSONObject, field: MongoFieldMeta, e: A): Unit =
    if (!field.ignored) {
      if (field.embedded)
        toMongo(e) match {
          case dbo2: BSONObject => dbo.putAll(dbo2)
          case MongoNothing => ()
          case x => dbo.put(field.name, x)
        }
      else
        toMongo(e) match {
          case MongoNothing => ()
          case x => dbo.put(field.name, x)
        }

    }

  private def readField[A: MongoFormat](f: MongoFieldMeta, dbo: BSONObject): A = {
    val mf = MongoFormat[A]
    def default = f.default.asInstanceOf[Option[A]].orElse(mf.default)
    if (f.ignored)
      default.getOrElse {
        throw new Exception("Missing default for ignored field '%s'.".format(f.name))
      }
    else if (f.embedded) mf.fromMongoValue(dbo)
    else {
      val value = dbo.get(f.name)
      if (value != null) mf.fromMongoValue(value)
      else {
        default.getOrElse {
          throw new Exception("Missing required field '%s' on deserialization.".format(f.name))
        }
      }
    }
  }

  private def findTypeValue(dbo: BSONObject, typeField: String): Option[String] =
    Option(dbo.get(typeField)).map(_.toString)

  private def typeSelector[A: ClassTag: MongoFormat](): TypeSelector[_] = {
    val clazz = classTag[A].runtimeClass
    val (typeField, typeValue) = getMongoClassMeta(clazz).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(clazz))
    }
    new TypeSelector[A](typeValue, clazz)
  }

  private def defaultTypeValue(clazz: Class[_]): String =
    clazz.getSimpleName.replace("$", "")
}
