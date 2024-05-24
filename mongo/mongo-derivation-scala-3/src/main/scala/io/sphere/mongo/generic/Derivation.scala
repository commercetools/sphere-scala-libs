package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import org.bson.BSONObject
import org.bson.types.ObjectId

import java.util.UUID
import java.util.regex.Pattern
import scala.deriving.Mirror
import scala.quoted.*

object MongoNothing
type SimpleMongoType = UUID | String | ObjectId | Short | Int | Long | Float | Double | Boolean |
  Pattern
type MongoType = BasicDBObject | SimpleMongoType | MongoNothing.type

trait TypedMongoFormat[A] extends Serializable {
  def toMongoValue(a: A): MongoType
  def fromMongoValue(mongoType: MongoType): A

//  /** needed JSON fields - ignored if empty */
  val fieldNames: Vector[String] = TypedMongoFormat.emptyFieldsSet

  def default: Option[A] = None
}

private final class NativeMongoFormat[A <: SimpleMongoType] extends TypedMongoFormat[A] {
  def toMongoValue(a: A): MongoType = a
  def fromMongoValue(any: MongoType): A = any.asInstanceOf[A]
}

inline def deriveMongoFormat[A](using Mirror.Of[A]): TypedMongoFormat[A] = TypedMongoFormat.derived

object TypedMongoFormat:
  inline def apply[A: TypedMongoFormat]: TypedMongoFormat[A] = summon

  private val emptyFieldsSet: Vector[String] = Vector.empty

  inline given derived[A](using Mirror.Of[A]): TypedMongoFormat[A] = Derivation.derived

  private def addField(bson: BasicDBObject, field: Field, mongoType: MongoType) =
    mongoType match
      case s: SimpleMongoType => bson.put(field.name, s)
      case innerBson: BasicDBObject =>
        if (field.embedded) innerBson.entrySet().forEach(p => bson.put(p.getKey, p.getValue))
        else bson.put(field.name, innerBson)
      case MongoNothing =>

  private object Derivation:
    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): TypedMongoFormat[A] =
      inline m match
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): TypedMongoFormat[A] =
      new TypedMongoFormat[A]:
        private val traitMetaData = AnnotationReader.readTraitMetaData[A]
        private val typeHintMap = traitMetaData.subtypes.collect {
          case (name, classMeta) if classMeta.typeHint.isDefined =>
            name -> classMeta.typeHint.get
        }
        private val reverseTypeHintMap = typeHintMap.map((on, n) => (n, on))
        private val formatters = summonFormatters[mirrorOfSum.MirroredElemTypes]
        private val names = constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
          .asInstanceOf[Vector[String]]
        private val formattersByTypeName = names.zip(formatters).toMap

        override def toMongoValue(a: A): MongoType =
          // we never get a trait here, only classes, it's safe to assume Product
          val originalTypeName = a.asInstanceOf[Product].productPrefix
          val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
          val bson =
            formattersByTypeName(originalTypeName).toMongoValue(a).asInstanceOf[BasicDBObject]
          bson.put(traitMetaData.typeDiscriminator, typeName)
          bson

        override def fromMongoValue(bson: MongoType): A =
          bson match
            case bson: BasicDBObject =>
              val typeName = bson.get(traitMetaData.typeDiscriminator).asInstanceOf[String]
              val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
              formattersByTypeName(originalTypeName).fromMongoValue(bson).asInstanceOf[A]
            case x =>
              throw new Exception(s"BsonObject is expected for a Trait subtype, instead got $x")
    end deriveTrait

    inline private def deriveCaseClass[A](
        mirrorOfProduct: Mirror.ProductOf[A]): TypedMongoFormat[A] =
      new TypedMongoFormat[A]:
        private val caseClassMetaData = AnnotationReader.readCaseClassMetaData[A]
        private val formatters = summonFormatters[mirrorOfProduct.MirroredElemTypes]
        private val fieldsAndFormatters = caseClassMetaData.fields.zip(formatters)

        override val fieldNames: Vector[String] = fieldsAndFormatters.flatMap((field, formatter) =>
          if (field.embedded) formatter.fieldNames :+ field.rawName
          else Vector(field.rawName))

        override def toMongoValue(a: A): MongoType =
          val bson = new BasicDBObject()
          val values = a.asInstanceOf[Product].productIterator
          formatters.zip(values).zip(caseClassMetaData.fields).foreach {
            case ((format, value), field) =>
              addField(bson, field, format.toMongoValue(value))
          }
          bson

        override def fromMongoValue(mongoType: MongoType): A =
          mongoType match
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
                    if (value ne null) format.fromMongoValue(value.asInstanceOf[MongoType])
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
    end deriveCaseClass

    inline private def summonFormatters[T <: Tuple]: Vector[TypedMongoFormat[Any]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[TypedMongoFormat[t]]
            .asInstanceOf[TypedMongoFormat[Any]] +: summonFormatters[ts]
