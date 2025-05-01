package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import io.sphere.mongo.format.MongoFormat
import org.bson.BSONObject

import scala.compiletime.{erasedValue, error, summonInline}

def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
  def toMongoValue(a: e.Value): Any = a.toString

  def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
}

inline def mongoTypeSwitch[SuperType, SubTypeTuple <: Tuple](): MongoFormat[SuperType] = {
  val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
  val typeHintMap = traitMetaData.subTypeTypeHints
  val reverseTypeHintMap = typeHintMap.map((on, n) => (n, on))
  val formatters: Vector[MongoFormat[Any]] = summonFormatters[SubTypeTuple]()
  val names = summonMetaData[SubTypeTuple]().map(_.name)
  val formattersByTypeName = names.zip(formatters).toMap

  MongoFormat.instance(
    toMongo = { a =>
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
        formattersByTypeName(originalTypeName).fromMongoValue(bson).asInstanceOf[SuperType]
      case x =>
        throw new Exception(s"BsonObject is expected for a Trait subtype, instead got $x")
    }
  )
}

private def findTypeValue(dbo: BSONObject, typeField: String): Option[String] =
  Option(dbo.get(typeField)).map(_.toString)

inline private def summonMetaData[T <: Tuple](
    acc: Vector[TypeMetaData] = Vector.empty): Vector[TypeMetaData] =
  inline erasedValue[T] match {
    case _: EmptyTuple => acc
    case _: (t *: ts) =>
      summonMetaData[ts](acc :+ AnnotationReader.readTypeMetaData[t])
  }

inline private def summonFormatters[T <: Tuple](
    acc: Vector[MongoFormat[Any]] = Vector.empty): Vector[MongoFormat[Any]] =
  inline erasedValue[T] match {
    case _: EmptyTuple => acc
    case _: (t *: ts) =>
      val headFormatter = summonInline[MongoFormat[t]].asInstanceOf[MongoFormat[Any]]
      summonFormatters[ts](acc :+ headFormatter)
  }
