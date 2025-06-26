package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import io.sphere.mongo.format.{MongoFormat, TraitMongoFormat}
import io.sphere.util.TypeMetaData
import org.bson.BSONObject

import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, erasedValue, error, summonInline}

inline def deriveMongoFormat[A](using Mirror.Of[A]): MongoFormat[A] = MongoFormat.derived

def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
  def toMongoValue(a: e.Value): Any = a.toString

  def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
}

inline def mongoTypeSwitch[SuperType, SubTypeTuple <: Tuple]: MongoFormat[SuperType] = {
  val traitMetaData = MongoAnnotationReader.readTraitMetaData[SuperType]
  val typeHintMap = traitMetaData.subTypeSerializedTypeNames
  val reverseTypeHintMap = typeHintMap.map((on, n) => (n, on))
  val formatters = summonFormatters[SubTypeTuple]()
  val subTypeNames = summonMetaData[SubTypeTuple]()

  val pairedFormatterWithSubtypeName = subTypeNames.map(_.scalaName).zip(formatters)
  val (caseClassFormatterList, traitFormatters) = pairedFormatterWithSubtypeName.partitionMap {
    case kv @ (name, formatter) =>
      formatter match {
        case traitFormatter: TraitMongoFormat[_] => Right(traitFormatter)
        case _ => Left(kv)
      }
  }
  val caseClassFormatters = caseClassFormatterList.toMap

  TraitMongoFormat.instance(
    toMongo = { a =>
      traitFormatters.view.map(_.attemptWrite(a)).find(_.isSuccess).map(_.get) match {
        case Some(bson) => bson
        case None =>
          val scalaTypeName = a.asInstanceOf[Product].productPrefix
          val serializedTypeName = typeHintMap.getOrElse(scalaTypeName, scalaTypeName)
          val bson =
            caseClassFormatters(scalaTypeName).toMongoValue(a).asInstanceOf[BasicDBObject]
          bson.put(traitMetaData.typeDiscriminator, serializedTypeName)
          bson
      }
    },
    fromMongo = {
      case bson: BasicDBObject =>
        traitFormatters.view.map(_.attemptRead(bson)).find(_.isSuccess).map(_.get) match {
          case Some(a) => a.asInstanceOf[SuperType]
          case None =>
            val serializedTypeName = bson.get(traitMetaData.typeDiscriminator).asInstanceOf[String]
            val scalaTypeName = reverseTypeHintMap.getOrElse(serializedTypeName, serializedTypeName)
            caseClassFormatters(scalaTypeName).fromMongoValue(bson).asInstanceOf[SuperType]
        }
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
      summonMetaData[ts](acc :+ MongoAnnotationReader.readTypeMetaData[t])
  }

inline private def summonFormatters[T <: Tuple](
    acc: Vector[MongoFormat[Any]] = Vector.empty): Vector[MongoFormat[Any]] =
  inline erasedValue[T] match {
    case _: EmptyTuple => acc
    case _: (t *: ts) =>
      val headFormatter = summonInline[MongoFormat[t]].asInstanceOf[MongoFormat[Any]]
      summonFormatters[ts](acc :+ headFormatter)
  }
