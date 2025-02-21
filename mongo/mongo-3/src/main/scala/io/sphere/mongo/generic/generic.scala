package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import io.sphere.mongo.format.MongoFormat
import org.bson.BSONObject

import scala.compiletime.{erasedValue, summonInline, error}

case object generic {
  inline def mongoTypeSwitch[SuperType, SubTypeTuple <: Tuple](): MongoFormat[SuperType] =
    new MongoFormat[SuperType] {
      failIfAnySubTypeIsNotAProduct[SubTypeTuple]
      private val traitMetaData = AnnotationReader.readTraitMetaData[SuperType]
      private val typeHintMap = traitMetaData.subTypeTypeHints
      private val reverseTypeHintMap = typeHintMap.map((on, n) => (n, on))
      private val formatters: Vector[MongoFormat[Any]] = summonFormatters[SubTypeTuple]()
      private val names = summonMetaData[SubTypeTuple]().map(_.name)
      private val formattersByTypeName = names.zip(formatters).toMap

      override def toMongoValue(a: SuperType): Any = {
        val originalTypeName = a.asInstanceOf[Product].productPrefix
        val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
        val bson =
          formattersByTypeName(originalTypeName).toMongoValue(a).asInstanceOf[BasicDBObject]
        bson.put(traitMetaData.typeDiscriminator, typeName)
        bson
      }

      override def fromMongoValue(bson: Any): SuperType =
        bson match {
          case bson: BasicDBObject =>
            val typeName = bson.get(traitMetaData.typeDiscriminator).asInstanceOf[String]
            val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
            formattersByTypeName(originalTypeName).fromMongoValue(bson).asInstanceOf[SuperType]
          case x =>
            throw new Exception(s"BsonObject is expected for a Trait subtype, instead got $x")
        }
    }

  private def findTypeValue(dbo: BSONObject, typeField: String): Option[String] =
    Option(dbo.get(typeField)).map(_.toString)

  inline private def failIfAnySubTypeIsNotAProduct[T <: Tuple]: Unit =
    inline erasedValue[T] match {
      case _: EmptyTuple => ()
      case _: (t *: ts) =>
        inline erasedValue[t] match {
          case _: Product => failIfAnySubTypeIsNotAProduct[ts]
          case _ => error("All types should be subtypes of Product")
        }
    }

  inline private def summonMetaData[T <: Tuple](
      acc: Vector[CaseClassMetaData] = Vector.empty): Vector[CaseClassMetaData] =
    inline erasedValue[T] match {
      case _: EmptyTuple => acc
      case _: (t *: ts) =>
        summonMetaData[ts](acc :+ AnnotationReader.readCaseClassMetaData[t])
    }

  inline private def summonFormatters[T <: Tuple](
      acc: Vector[MongoFormat[Any]] = Vector.empty): Vector[MongoFormat[Any]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => acc
      case _: (t *: ts) =>
        val headFormatter = summonInline[MongoFormat[t]].asInstanceOf[MongoFormat[Any]]
        summonFormatters[ts](acc :+ headFormatter)
    }

}
