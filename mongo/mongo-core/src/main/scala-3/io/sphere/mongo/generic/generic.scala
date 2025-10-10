package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import io.sphere.mongo.format.{MongoFormat, TraitMongoFormat}
import io.sphere.util.TypeMetaData
import org.bson.BSONObject

import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, erasedValue, error, summonInline}
import scala.reflect.ClassTag

inline def deriveMongoFormat[A](using Mirror.Of[A]): MongoFormat[A] = MongoFormat.derived

def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
  def toMongoValue(a: e.Value): Any = a.toString

  def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
}

inline def mongoTypeSwitch[SuperType, SubTypeTuple <: Tuple]: MongoFormat[SuperType] = {
  val traitMetaData = MongoAnnotationReader.readTraitMetaData[SuperType]
  println(s"--> $traitMetaData")
  val typeHintMap = traitMetaData.serializedNamesOfSubTypes
  val formatters = summonFormatters[SubTypeTuple]()
  val subTypeMetaData = summonMetaData[SubTypeTuple]()

  val pairedFormatterWithSubtypeName = subTypeMetaData.map(_.scalaName).zip(formatters)
  val (caseClassFormatterList, traitFormatters) = pairedFormatterWithSubtypeName.partitionMap {
    case (scalaName, formatterEither) =>
      formatterEither match {
        case Right(traitFormatter: TraitMongoFormat[_]) =>
          println(s"trait case -${traitFormatter}")
          Right(traitFormatter)

        case Left(clazz, formatter) =>
          println(s"class case -$clazz")
          val serializedName = typeHintMap.getOrElse(scalaName, scalaName)
          val readFormat = serializedName -> formatter
          val writeFormat = clazz -> formatter.mapToMongo { bson =>
            bson.asInstanceOf[BasicDBObject].put(traitMetaData.typeDiscriminator, serializedName)
            bson
          }
          Left((readFormat, writeFormat))
      }
  }

  val (ownReadFormatters, ownWriteFormatters) = caseClassFormatterList.unzip
  val childReadFormatters = traitFormatters.map(_.readFormatters).fold(Map.empty)(_ ++ _)
  val childWriteFormatters = traitFormatters.map(_.writeFormatters).fold(Map.empty)(_ ++ _)

  val allReadFormatters =
    (childReadFormatters ++ ownReadFormatters).asInstanceOf[Map[String, MongoFormat[SuperType]]]
  val allWriteFormatters =
    (childWriteFormatters ++ ownWriteFormatters).asInstanceOf[Map[Class[_], MongoFormat[SuperType]]]

  println(s">>>> write ${allWriteFormatters.map(_._1.toString)}")
  println(s"<<<< read  ${allReadFormatters.map(_._1)}")

  TraitMongoFormat.instance(
    toMongo = { a =>
      println(s">>> pname ${a.asInstanceOf[Product].productPrefix}")
      allWriteFormatters(a.getClass).toMongoValue(a)
    },
    fromMongo = {
      case bson: BasicDBObject =>
        val serializedTypeName = bson.get(traitMetaData.typeDiscriminator).asInstanceOf[String]
        println(s";;;;; ${traitMetaData.typeDiscriminator} -> $serializedTypeName")
        allReadFormatters(serializedTypeName).fromMongoValue(bson)
      case x =>
        throw new Exception(s"BsonObject is expected for a Trait subtype, instead got $x")
    },
    readFormattersPassedToParent = allReadFormatters,
    writeFormattersPassedToParent = allWriteFormatters,
    typeDiscriminatorPassedToParent = traitMetaData.typeDiscriminator
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

private type FormatterAlias = Either[(Class[_], MongoFormat[Any]), TraitMongoFormat[Any]]

inline private def summonFormatters[T <: Tuple](
    acc: Vector[FormatterAlias] = Vector.empty): Vector[FormatterAlias] =
  inline erasedValue[T] match {
    case _: EmptyTuple => acc
    case _: (t *: ts) =>
      summonInline[MongoFormat[t]].asInstanceOf[MongoFormat[Any]] match {
        case traitFormat: TraitMongoFormat[_] => summonFormatters[ts](acc :+ Right(traitFormat))

        case caseClassFormat =>
          val clazz = summonInline[ClassTag[t]].runtimeClass
          summonFormatters[ts](acc :+ Left(clazz -> caseClassFormat))
      }

  }
