package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import org.bson.types.ObjectId

import java.util.UUID
import java.util.regex.Pattern
import scala.deriving.Mirror

type SimpleMongoType = UUID | String | ObjectId | Short | Int | Long | Float | Double | Boolean |
  Pattern
type MongoType = BasicDBObject | SimpleMongoType

trait TypedMongoFormat[A] extends Serializable {
  def toMongoValue(a: A): MongoType
  def fromMongoValue(mongoType: MongoType): A
}

private final class NativeMongoFormat[A <: SimpleMongoType] extends TypedMongoFormat[A] {
  def toMongoValue(a: A): MongoType = a
  def fromMongoValue(any: MongoType): A = any.asInstanceOf[A]
}

object TypedMongoFormat:
  inline def apply[A: TypedMongoFormat]: TypedMongoFormat[A] = summon

  inline given derive[A](using Mirror.Of[A]): TypedMongoFormat[A] = Derivation.derived

  given TypedMongoFormat[Int] = new NativeMongoFormat[Int]
  given TypedMongoFormat[String] = new NativeMongoFormat[String]
  given TypedMongoFormat[Boolean] = new NativeMongoFormat[Boolean]

  private object Derivation:
    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): TypedMongoFormat[A] =
      inline m match
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): TypedMongoFormat[A] =
      new TypedMongoFormat[A]:
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
            case _ => throw new Exception("idk yet")
    end deriveTrait

    inline def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): TypedMongoFormat[A] =
      new TypedMongoFormat[A]:
        val formatters = summonFormatters[mirrorOfProduct.MirroredElemTypes]
        val fieldNames =
          constValueTuple[mirrorOfProduct.MirroredElemLabels].productIterator.toVector
            .asInstanceOf[Vector[String]]

        override def toMongoValue(a: A): MongoType =
          val bson = new BasicDBObject()
          val values = a.asInstanceOf[Product].productIterator
          formatters.zip(values).zip(fieldNames).foreach { case ((format, value), fieldName) =>
            bson.put(fieldName, format.toMongoValue(value))
          }
          bson

        override def fromMongoValue(mongoType: MongoType): A =
          mongoType match
            case bson: BasicDBObject =>
              val fieldsAsAList = fieldNames
                .zip(formatters)
                .map((fieldName, format) =>
                  format.fromMongoValue(bson.get(fieldName).asInstanceOf[MongoType]))
              val tuple = Tuple.fromArray(fieldsAsAList.toArray)
              mirrorOfProduct.fromTuple(tuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

            case _ => throw new Exception("not a Map")
    end deriveCaseClass

    inline private def summonFormatters[T <: Tuple]: Vector[TypedMongoFormat[Any]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[TypedMongoFormat[t]]
            .asInstanceOf[TypedMongoFormat[Any]] +: summonFormatters[ts]
