package io.sphere.mongo.format

import com.mongodb.BasicDBObject
import io.sphere.mongo.generic.{MongoAnnotationReader, mongoTypeSwitch}
import io.sphere.util.Field
import org.bson.BSONObject
import org.bson.types.ObjectId

import java.util.UUID
import java.util.regex.Pattern
import scala.deriving.Mirror
import scala.util.control.NonFatal

type SimpleMongoType = UUID | String | ObjectId | Short | Int | Long | Float | Double | Boolean |
  Pattern

trait MongoFormat[A] extends Serializable {
  def toMongoValue(a: A): Any
  def fromMongoValue(mongoType: Any): A

  //  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = MongoFormat.emptyFields

  def default: Option[A] = None

  final def mapToMongo(bson: Any => Any): MongoFormat[A] =
    MongoFormat.instance(
      fromMongo = fromMongoValue,
      toMongo = toMongoValue.andThen(bson),
      fieldSet = fields)
}

/** Some extra information for traits and abstract classes so we can handle nested hierarchies
  * easier
  */
trait TraitMongoFormat[A] extends MongoFormat[A] {
  val readFormatters: Map[String, MongoFormat[A]]
  val writeFormatters: Map[Class[_], MongoFormat[A]]
}

object TraitMongoFormat {

  def instance[A](
      fromMongo: Any => A,
      toMongo: A => Any,
      readFormattersPassedToParent: Map[String, MongoFormat[A]],
      writeFormattersPassedToParent: Map[Class[_], MongoFormat[A]]): TraitMongoFormat[A] = new {
    override def toMongoValue(a: A): Any = toMongo(a)
    override def fromMongoValue(mongoType: Any): A = fromMongo(mongoType)
    override val readFormatters: Map[String, MongoFormat[A]] = readFormattersPassedToParent
    override val writeFormatters: Map[Class[_], MongoFormat[A]] = writeFormattersPassedToParent
  }
}

object MongoFormat {
  private val emptyFields: Set[String] = Set.empty

  inline def apply[A: MongoFormat]: MongoFormat[A] = summon
  inline given derived[A](using Mirror.Of[A]): MongoFormat[A] = Derivation.derived

  def instance[A](
      fromMongo: Any => A,
      toMongo: A => Any,
      fieldSet: Set[String] = emptyFields): MongoFormat[A] = new {

    override def toMongoValue(a: A): Any = toMongo(a)
    override def fromMongoValue(mongoType: Any): A = fromMongo(mongoType)
    override val fields: Set[String] = fieldSet
  }

  private object Derivation {
    import scala.compiletime.{erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): MongoFormat[A] =
      inline m match {
        case s: Mirror.SumOf[A] => mongoTypeSwitch[A, s.MirroredElemTypes]
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): MongoFormat[A] = {
      val caseClassMetaData = MongoAnnotationReader.readTypeMetaData[A]
      val formatters = summonFormatters[mirrorOfProduct.MirroredElemTypes]
      val fieldsAndFormatters = caseClassMetaData.fields.zip(formatters)

      val fields: Set[String] = fieldsAndFormatters.toSet.flatMap((field, formatter) =>
        if (field.embedded) formatter.fields + field.scalaName
        else Set(field.scalaName))

      instance(
        toMongo = { a =>
          val bson = new BasicDBObject()
          val values = a.asInstanceOf[Product].productIterator
          formatters.zip(values).zip(caseClassMetaData.fields).foreach {
            case ((format, value), field) =>
              addField(bson, field, format.toMongoValue(value))
          }
          bson
        },
        fromMongo = {
          case bson: BasicDBObject =>
            val valuesOfClass = fieldsAndFormatters.map(readField(bson))
            val tuple = Tuple.fromArray(valuesOfClass.toArray)
            mirrorOfProduct.fromTuple(tuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

          case x => throw new Exception(s"DBObject expected but got ${x}.")
        },
        fieldSet = fields
      )
    }

    inline private def summonFormatters[T <: Tuple]: Vector[MongoFormat[Any]] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[MongoFormat[t]]
            .asInstanceOf[MongoFormat[Any]] +: summonFormatters[ts]
      }

  }

  private def addField(bson: BasicDBObject, field: Field, mongoType: Any): Unit =
    if (!field.ignored)
      mongoType match {
        case s: SimpleMongoType => bson.put(field.serializedName, s)
        case innerBson: BasicDBObject =>
          if (field.embedded) innerBson.entrySet().forEach(p => bson.put(p.getKey, p.getValue))
          else bson.put(field.serializedName, innerBson)
        case MongoNothing =>
      }

  private def readField(bson: BSONObject)(field: Field, format: MongoFormat[Any]): Any = {
    def defaultValue = field.defaultArgument.orElse(format.default)
    if (field.ignored)
      defaultValue.getOrElse {
        throw new Exception(s"Missing default for ignored field '${field.serializedName}'.")
      }
    else if (field.embedded) format.fromMongoValue(bson)
    else {
      val value = bson.get(field.serializedName)
      if (value ne null)
        try format.fromMongoValue(value.asInstanceOf[Any])
        catch {
          case NonFatal(e) =>
            throw new Exception(s"Could not deserialize field '${field.serializedName}'", e)
        }
      else
        defaultValue.getOrElse {
          throw new Exception(
            s"Missing required field '${field.serializedName}' on deserialization.")
        }
    }
  }
}
