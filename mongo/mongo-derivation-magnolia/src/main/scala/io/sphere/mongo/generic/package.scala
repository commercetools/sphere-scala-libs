package io.sphere.mongo

import com.mongodb.{BasicDBObject, DBObject}
import io.sphere.mongo.format.{MongoFormat, MongoNothing, toMongo}
import io.sphere.util.{Logging, Memoizer}
import magnolia._
import org.bson.BSONObject

import scala.language.experimental.macros

package object generic extends Logging {

  /**
  * Creates a MongoFormat instance for an Enumeration type that encodes the `toString`
  * representations of the enumeration values.
  */
  def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
    def toMongoValue(a: e.Value): Any = a.toString
    def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
  }

  type Typeclass[T] = MongoFormat[T]

  def deriveMongoFormat[T]: MongoFormat[T] = macro Magnolia.gen[T]

  private val addNoTypeHint: DBObject => Unit = Function.const(())

  def combine[T](caseClass: CaseClass[MongoFormat, T]): MongoFormat[T] = new MongoFormat[T] {
    private val mongoClass = getMongoClassMeta(caseClass)
    private val _fields = mongoClass.fields
    private val _withTypeHint = mongoClass.typeHint.isDefined
    private val addTypeHint: DBObject => Unit =
      mongoClass.typeHint.fold(addNoTypeHint)(th => dbo => dbo.put(th.field, th.value))

    override def toMongoValue(r: T): Any = {
      val dbo = new BasicDBObject
      if (_withTypeHint) addTypeHint(dbo)

      var i = 0
      caseClass.parameters.foreach { p =>
        writeField(dbo, _fields(i), p.dereference(r))(p.typeclass)
        i += 1
      }
      dbo
    }

    override def fromMongoValue(any: Any): T = any match {
      case dbo: DBObject =>
        var i = -1
        val fieldValues: Seq[Any] = caseClass.parameters.map { p =>
          i += 1
          readField(_fields(i), dbo)(p.typeclass)
        }
        caseClass.rawConstruct(fieldValues)
      case _ => sys.error("Deserialization failed. DBObject expected.")
    }

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

  def dispatch[T](sealedTrait: SealedTrait[MongoFormat, T]): MongoFormat[T] = new MongoFormat[T] {

    private val allSelectors = sealedTrait.subtypes.map { subType =>
      typeSelector(subType)
    }
    private val readMapBuilder = Map.newBuilder[String, TypeSelector[_]]
    private val writeMapBuilder = Map.newBuilder[TypeName, TypeSelector[_]]
    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
      writeMapBuilder += (s.subType.typeName -> s)
    }
    private val readMap = readMapBuilder.result
    private val writeMap = writeMapBuilder.result

    private val typeField = sealedTrait.annotations.collectFirst {
      case a: MongoTypeHintField => a.value
    }.getOrElse(defaultTypeFieldName)

    override def toMongoValue(t: T): Any = {
      sealedTrait.dispatch(t) { subtype =>
        writeMap.get(subtype.typeName) match {
          case None => new BasicDBObject(typeField, defaultTypeValue(subtype.typeName))
          case Some(w) => subtype.typeclass.toMongoValue(subtype.cast(t)) match {
            case dbo: BSONObject => findTypeValue(dbo, typeField) match {
              case Some(_) => dbo
              case None =>
                dbo.put(typeField, w.typeValue)
                dbo
            }
            case _ => throw new Exception("Excepted 'BSONObject'")
          }
        }
      }
    }

    override def fromMongoValue(any: Any): T = {
      any match {
        case dbo: BSONObject =>
          findTypeValue(dbo, typeField) match {
            case Some(t) => readMap.get(t) match {
              case Some(r) => r.subType.typeclass.fromMongoValue(dbo).asInstanceOf[T]
              case None => sys.error("Invalid type value '" + t + "' in DBObject '%s'.".format(dbo))
            }
            case None => sys.error("Missing type field '" + typeField + "' in DBObject '%s'.".format(dbo))
          }
        case _ => sys.error("DBObject expected.")
      }
    }
  }

  private val defaultTypeFieldName: String = MongoTypeHintField.defaultValue

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

  private val getMongoClassMeta = new Memoizer[CaseClass[MongoFormat, _], MongoClassMeta](caseClass => {
    def hintVal(h: generic.MongoTypeHint): String =
      if (h.value.isEmpty) defaultTypeValue(caseClass.typeName)
      else h.value

    log.trace("Initializing Mongo metadata for %s".format(caseClass.typeName.full))

    val annotations = caseClass.annotations

    val typeHintFieldAnnot: Option[MongoTypeHintField] = annotations.collectFirst {
      case h: MongoTypeHintField => h
    }
    val typeHintAnnot: Option[generic.MongoTypeHint] = annotations.collectFirst {
      case h: generic.MongoTypeHint => h
    }
    val typeField = typeHintFieldAnnot.map(_.value)
    val typeValue = typeHintAnnot.map(hintVal)

    MongoClassMeta(
      typeHint = (typeField, typeValue) match {
        case (Some(field), Some(hint)) => Some(MongoClassMeta.TypeHint(field, hint))
        case (None       , Some(hint)) => Some(MongoClassMeta.TypeHint(defaultTypeFieldName, hint))
        case (Some(field), None)       => Some(MongoClassMeta.TypeHint(field, defaultTypeValue(caseClass.typeName)))
        case (None       , None)       => None
      },
      fields = getMongoFieldMeta(caseClass)
    )
  })

  private val getMongoClassMetaFromSubType = new Memoizer[Subtype[MongoFormat, _], MongoClassMeta](subType => {
    def hintVal(h: generic.MongoTypeHint): String =
      if (h.value.isEmpty) defaultTypeValue(subType.typeName)
      else h.value

    log.trace("Initializing Mongo metadata for %s".format(subType.typeName.full))

    val annotations = subType.annotations

    val typeHintFieldAnnot: Option[MongoTypeHintField] = annotations.collectFirst {
      case h: MongoTypeHintField => h
    }
    val typeHintAnnot: Option[generic.MongoTypeHint] = annotations.collectFirst {
      case h: generic.MongoTypeHint => h
    }
    val typeField = typeHintFieldAnnot.map(_.value)
    val typeValue = typeHintAnnot.map(hintVal)

    MongoClassMeta(
      typeHint = (typeField, typeValue) match {
        case (Some(field), Some(hint)) => Some(MongoClassMeta.TypeHint(field, hint))
        case (None       , Some(hint)) => Some(MongoClassMeta.TypeHint(defaultTypeFieldName, hint))
        case (Some(field), None)       => Some(MongoClassMeta.TypeHint(field, defaultTypeValue(subType.typeName)))
        case (None       , None)       => None
      },
      fields = IndexedSeq[MongoFieldMeta]()
    )
  })

  private def getMongoFieldMeta(caseClass: CaseClass[MongoFormat, _]): IndexedSeq[MongoFieldMeta] = {
    caseClass.parameters.map { p =>
      val annotations = p.annotations
      val name = annotations.collectFirst {
        case h: MongoKey => h
      }.fold(p.label)(_.value)
      val embedded = annotations.exists {
        case _: MongoEmbedded => true
        case _ => false
      }
      val ignored = annotations.exists {
        case _: MongoIgnore => true
        case _ => false
      }
      if (ignored && p.default.isEmpty) {
        throw new Exception("Ignored Mongo field '%s' must have a default value.".format(p.label))
      }
      MongoFieldMeta(name, p.default, embedded, ignored)
    }.toIndexedSeq
  }

  private def writeField[A: MongoFormat](dbo: DBObject, field: MongoFieldMeta, e: A): Unit = {
    if (!field.ignored) {
      if (field.embedded)
        toMongo(e) match {
          case dbo2: DBObject => dbo.putAll(dbo2)
          case MongoNothing => ()
          case x => dbo.put(field.name, x)
        }
      else
        toMongo(e) match {
          case MongoNothing => ()
          case x => dbo.put(field.name, x)
        }
    }
  }

  private def readField[A: MongoFormat](f: MongoFieldMeta, dbo: DBObject): A = {
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

  private case class TypeSelector[A](val typeField: String, val typeValue: String, subType: Subtype[MongoFormat, A])

  private def typeSelector[A](subType: Subtype[MongoFormat, A]): TypeSelector[A] = {
    val (typeField, typeValue) = getMongoClassMetaFromSubType(subType).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(subType.typeName))
    }
    new TypeSelector[A](typeField, typeValue, subType)
  }

  private def defaultTypeValue(typeName: TypeName): String =
    typeName.short.replace("$", "")

}
