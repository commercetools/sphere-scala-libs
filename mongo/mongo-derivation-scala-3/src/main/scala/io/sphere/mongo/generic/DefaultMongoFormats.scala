package io.sphere.mongo.generic

import com.mongodb.BasicDBObject

object DefaultMongoFormats extends DefaultMongoFormats {}

trait DefaultMongoFormats {
  given TypedMongoFormat[Int] = new NativeMongoFormat[Int]

  given TypedMongoFormat[String] = new NativeMongoFormat[String]

  given TypedMongoFormat[Boolean] = new NativeMongoFormat[Boolean]

  given [A](using TypedMongoFormat[A]): TypedMongoFormat[Option[A]] =
    new TypedMongoFormat[Option[A]] {
      override def toMongoValue(a: Option[A]): MongoType =
        a match {
          case Some(value) => summon[TypedMongoFormat[A]].toMongoValue(value)
          case None => MongoNothing
        }

      override def fromMongoValue(mongoType: MongoType): Option[A] = {
        val fieldNames = summon[TypedMongoFormat[A]].fieldNames
        if (mongoType == null) None
        else
          mongoType match {
            case s: SimpleMongoType => Some(summon[TypedMongoFormat[A]].fromMongoValue(s))
            case bson: BasicDBObject =>
              val bsonFieldNames = bson.keySet().toArray
              if (fieldNames.nonEmpty && bsonFieldNames.intersect(fieldNames).isEmpty) None
              else Some(summon[TypedMongoFormat[A]].fromMongoValue(bson))
            case MongoNothing => None // This can't happen, but it makes the compiler happy
          }
      }

      override def default: Option[Option[A]] = Some(None)
    }
}
