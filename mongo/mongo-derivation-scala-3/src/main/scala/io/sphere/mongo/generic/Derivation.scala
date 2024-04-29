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
}

private final class NativeMongoFormat[A <: SimpleMongoType] extends TypedMongoFormat[A] {
  def toMongoValue(a: A): MongoType = a
  def fromMongoValue(any: MongoType): A = any.asInstanceOf[A]
}

inline def deriveMongoFormat[A: TypedMongoFormat]: TypedMongoFormat[A] = summon

object TypedMongoFormat:
  private val emptyFieldsSet: Vector[String] = Vector.empty
  inline def readCaseClassMetaData[T]: Annotations = ${ readCaseClassMetaDataImpl[T] }

  private def readCaseClassMetaDataImpl[T: Type](using Quotes): Expr[Annotations] =
    AnnotationReader().readCaseClassMetaData[T]

  inline given derive[A](using Mirror.Of[A]): TypedMongoFormat[A] = Derivation.derived

  given TypedMongoFormat[Int] = new NativeMongoFormat[Int]
  given TypedMongoFormat[String] = new NativeMongoFormat[String]
  given TypedMongoFormat[Boolean] = new NativeMongoFormat[Boolean]

  given [A](using TypedMongoFormat[A]): TypedMongoFormat[Option[A]] =
    new TypedMongoFormat[Option[A]]:
      override def toMongoValue(a: Option[A]): MongoType =
        a match
          case Some(value) => summon[TypedMongoFormat[A]].toMongoValue(value)
          case None => MongoNothing

      override def fromMongoValue(mongoType: MongoType): Option[A] =
        val fieldNames = summon[TypedMongoFormat[A]].fieldNames
        if (mongoType == null) None
        else
          mongoType match
            case s: SimpleMongoType => Some(summon[TypedMongoFormat[A]].fromMongoValue(s))
            case bson: BasicDBObject =>
              val bsonFieldNames = bson.keySet().toArray
              if (fieldNames.nonEmpty && bsonFieldNames.intersect(fieldNames).isEmpty) None
              else Some(summon[TypedMongoFormat[A]].fromMongoValue(bson))
            case MongoNothing => None // This can't happen, but it makes the compiler happy

  private def addField(bson: BasicDBObject, field: Field, mongoType: MongoType) =
    mongoType match
      case s: SimpleMongoType => bson.put(field.fieldName, s)
      case innerBson: BasicDBObject =>
        if (field.embedded) innerBson.entrySet().forEach(p => bson.put(p.getKey, p.getValue))
        else bson.put(field.fieldName, innerBson)
      case MongoNothing =>

  private object Derivation:
    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): TypedMongoFormat[A] =
      inline m match
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): TypedMongoFormat[A] =
      new TypedMongoFormat[A]:
        val annotations = readCaseClassMetaData[A]
        val typeField = "typeDiscriminator"
        val formatters = summonFormatters[mirrorOfSum.MirroredElemTypes]
        val names = constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
          .asInstanceOf[Vector[String]]
        val formattersByTypeName = names.zip(formatters).toMap

        override def toMongoValue(a: A): MongoType =
          // we never get a trait here, only classes, it's safe to assume Product
          val typeName = a.asInstanceOf[Product].productPrefix
          val bson = formattersByTypeName(typeName).toMongoValue(a).asInstanceOf[BasicDBObject]
          bson.put(typeField, typeName)
          bson

        override def fromMongoValue(bson: MongoType): A =
          bson match
            case bson: BasicDBObject =>
              val typeName = bson.get(typeField).asInstanceOf[String]
              formattersByTypeName(typeName).fromMongoValue(bson).asInstanceOf[A]
            case x =>
              throw new Exception(s"BsonObject is expected for a Trait subtype, instead got $x")
    end deriveTrait

    inline private def deriveCaseClass[A](
        mirrorOfProduct: Mirror.ProductOf[A]): TypedMongoFormat[A] =
      new TypedMongoFormat[A]:
        val caseClassMetaData = readCaseClassMetaData[A]
        val formatters = summonFormatters[mirrorOfProduct.MirroredElemTypes]
        val fieldsAndFormatters = caseClassMetaData.fields.zip(formatters)

        override val fieldNames: Vector[String] = fieldsAndFormatters.flatMap((field, formatter) =>
          if (field.embedded) formatter.fieldNames :+ field.name
          else Vector(field.name))

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
              val fieldsAsAList = fieldsAndFormatters
                .map { (field, format) =>
                  if (field.embedded)
                    format.fromMongoValue(bson)
                  else
                    format.fromMongoValue(bson.get(field.fieldName).asInstanceOf[MongoType])
                }
              val tuple = Tuple.fromArray(fieldsAsAList.toArray)
              mirrorOfProduct.fromTuple(tuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

            case x => throw new Exception(s"BsonObject is expected for a class, instead got: ${x}")
    end deriveCaseClass

    inline private def summonFormatters[T <: Tuple]: Vector[TypedMongoFormat[Any]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[TypedMongoFormat[t]]
            .asInstanceOf[TypedMongoFormat[Any]] +: summonFormatters[ts]
