package io.sphere.mongo.format

import com.mongodb.BasicDBObject
import io.sphere.mongo.generic.{AnnotationReader, Field, TraitMetaData}
import org.bson.types.ObjectId

import java.util.UUID
import java.util.regex.Pattern
import scala.deriving.Mirror

object MongoNothing

type SimpleMongoType = UUID | String | ObjectId | Short | Int | Long | Float | Double | Boolean |
  Pattern

trait MongoFormat[A] extends Serializable {
  def toMongoValue(a: A): Any
  def fromMongoValue(mongoType: Any): A

  //  /** needed JSON fields - ignored if empty */
  val fieldNames: Vector[String] = MongoFormat.emptyFields

  def default: Option[A] = None
}
final class NativeMongoFormat[A] extends MongoFormat[A] {
  def toMongoValue(a: A): Any = a
  def fromMongoValue(any: Any): A = any.asInstanceOf[A]
}

inline def deriveMongoFormat[A](using Mirror.Of[A]): MongoFormat[A] = MongoFormat.derived

object MongoFormat {
  inline def apply[A: MongoFormat]: MongoFormat[A] = summon

  private val emptyFields: Vector[String] = Vector.empty

  inline given derived[A](using Mirror.Of[A]): MongoFormat[A] = Derivation.derived

  private def addField(bson: BasicDBObject, field: Field, mongoType: Any): Unit =
    if (!field.ignored)
      mongoType match {
        case s: SimpleMongoType => bson.put(field.name, s)
        case innerBson: BasicDBObject =>
          if (field.embedded) innerBson.entrySet().forEach(p => bson.put(p.getKey, p.getValue))
          else bson.put(field.name, innerBson)
        case MongoNothing =>
      }

  private object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): MongoFormat[A] =
      inline m match {
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): MongoFormat[A] = {
      val traitMetaData = AnnotationReader.readTraitMetaData[A]
      val typeHintMap = traitMetaData.subTypeTypeHints
      val reverseTypeHintMap = typeHintMap.map((on, n) => (n, on))
      val formatters = summonFormatters[mirrorOfSum.MirroredElemTypes]
      val names = constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
        .asInstanceOf[Vector[String]]
      val formattersByTypeName = names.zip(formatters).toMap

      MongoFormat.create[A](
        toMongo = { a =>
          // we never get a trait here, only classes, it's safe to assume Product
          val originalTypeName = a.asInstanceOf[Product].productPrefix
          val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
          val bson =
            formattersByTypeName(originalTypeName).toMongoValue(a).asInstanceOf[BasicDBObject]
          bson.put(traitMetaData.typeDiscriminator, typeName)
          bson
        },
        fromMongo = {
          case bson: BasicDBObject =>
            val typeName = bson.get(traitMetaData.typeDiscriminator).asInstanceOf[String]
            val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
            formattersByTypeName(originalTypeName).fromMongoValue(bson).asInstanceOf[A]
          case x =>
            throw new Exception(s"BsonObject is expected for a Trait subtype, instead got $x")
        }
      )
    }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): MongoFormat[A] = {
      val caseClassMetaData = AnnotationReader.readCaseClassMetaData[A]
      val formatters = summonFormatters[mirrorOfProduct.MirroredElemTypes]
      val fieldsAndFormatters = caseClassMetaData.fields.zip(formatters)

      MongoFormat.create(
        fields = fieldsAndFormatters.flatMap((field, formatter) =>
          if (field.embedded) formatter.fieldNames :+ field.rawName
          else Vector(field.rawName)),
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
            val fields = fieldsAndFormatters
              .map { (field, format) =>
                def defaultValue = field.defaultArgument.orElse(format.default)

                if (field.ignored)
                  defaultValue.getOrElse {
                    throw new Exception(
                      s"Missing default parameter value for ignored field `${field.name}` on deserialization.")
                  }
                else if (field.embedded) format.fromMongoValue(bson)
                else {
                  val value = bson.get(field.name)
                  if (value ne null) format.fromMongoValue(value.asInstanceOf[Any])
                  else
                    defaultValue.getOrElse {
                      throw new Exception(
                        s"Missing required field '${field.name}' on deserialization.")
                    }
                }
              }
            val tuple = Tuple.fromArray(fields.toArray)
            mirrorOfProduct.fromTuple(tuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

          case x => throw new Exception(s"BasicDBObject is expected for a class, instead got: $x")
        }
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

  // This is needed to remove the "New anonymous class definition will be duplicated at each inline site" warnings
  private def create[A](
      toMongo: A => Any,
      fromMongo: Any => A,
      fields: Vector[String] = MongoFormat.emptyFields): MongoFormat[A] =
    new MongoFormat[A] {
      override def toMongoValue(a: A): Any = toMongo(a)

      override def fromMongoValue(mongoType: Any): A = fromMongo(mongoType)

      override val fieldNames: Vector[String] = fields
    }
}
