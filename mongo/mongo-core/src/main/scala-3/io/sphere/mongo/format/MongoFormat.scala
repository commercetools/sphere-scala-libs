package io.sphere.mongo.format

import com.mongodb.BasicDBObject
import io.sphere.mongo.generic.{AnnotationReader, Field}
import io.sphere.util.VectorUtils.*
import org.bson.types.ObjectId

import java.util.UUID
import java.util.regex.Pattern
import scala.deriving.Mirror

type SimpleMongoType = UUID | String | ObjectId | Short | Int | Long | Float | Double | Boolean |
  Pattern

trait MongoFormat[A] extends Serializable {
  def toMongoValue(a: A): Any
  def fromMongoValue(mongoType: Any): A

  //  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = MongoFormat.emptyFields

  def default: Option[A] = None
}

/** Some extra information for traits and abstract classes so we can handle nested hierarchies
  * easier
  */
trait TraitMongoFormat[A] extends MongoFormat[A] {
  val subTypeNames: Vector[String]
  val typeDiscriminator: String
}

object TraitMongoFormat {
  def instance[A](
      fromMongo: Any => A,
      toMongo: A => Any,
      subTypes: Vector[String],
      typeDiscr: String): TraitMongoFormat[A] = new {
    override def toMongoValue(a: A): Any = toMongo(a)
    override def fromMongoValue(mongoType: Any): A = fromMongo(mongoType)
    override val subTypeNames: Vector[String] = subTypes
    override val typeDiscriminator: String = typeDiscr
  }
}

inline def deriveMongoFormat[A](using Mirror.Of[A]): MongoFormat[A] = MongoFormat.derived

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
    import scala.compiletime.{constValue, constValueTuple, erasedValue, error, summonInline}

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
      val formattersByTypeName = names
        .zip(formatters)
        .flatMap { case kv @ (name, formatter) =>
          formatter match {
            case traitFormatter: TraitMongoFormat[_] =>
              traitFormatter.subTypeNames.map(_ -> formatter)
            case _ => Vector(kv)
          }
        }
        .toMapWithNoDuplicateKeys

      TraitMongoFormat.instance(
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
        },
        subTypes = names,
        typeDiscr = traitMetaData.typeDiscriminator
      )
    }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): MongoFormat[A] = {
      val caseClassMetaData = AnnotationReader.readTypeMetaData[A]
      val formatters = summonFormatters[mirrorOfProduct.MirroredElemTypes]
      val fieldsAndFormatters = caseClassMetaData.fields.zip(formatters)

      val fields: Set[String] = fieldsAndFormatters.toSet.flatMap((field, formatter) =>
        if (field.embedded) formatter.fields + field.rawName
        else Set(field.rawName))

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
            val fields = fieldsAndFormatters
              .map { case (field, format) =>
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
}
