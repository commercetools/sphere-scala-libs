package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import io.sphere.mongo.format.{MongoFormat, MongoNothing, NativeMongoFormat, SimpleMongoType}

object DefaultMongoFormats extends DefaultMongoFormats {}

trait DefaultMongoFormats {
  given MongoFormat[Int] = new NativeMongoFormat[Int]

  given MongoFormat[String] = new NativeMongoFormat[String]

  given MongoFormat[Boolean] = new NativeMongoFormat[Boolean]

  given [A](using MongoFormat[A]): MongoFormat[Option[A]] =
    new MongoFormat[Option[A]] {
      override def toMongoValue(a: Option[A]): Any =
        a match {
          case Some(value) => summon[MongoFormat[A]].toMongoValue(value)
          case None => MongoNothing
        }

      override def fromMongoValue(mongoType: Any): Option[A] = {
        val fieldNames = summon[MongoFormat[A]].fieldNames
        if (mongoType == null) None
        else
          mongoType match {
            case s: SimpleMongoType => Some(summon[MongoFormat[A]].fromMongoValue(s))
            case bson: BasicDBObject =>
              val bsonFieldNames = bson.keySet().toArray
              if (fieldNames.nonEmpty && bsonFieldNames.intersect(fieldNames).isEmpty) None
              else Some(summon[MongoFormat[A]].fromMongoValue(bson))
            case MongoNothing => None // This can't happen, but it makes the compiler happy
          }
      }

      override def default: Option[Option[A]] = Some(None)
    }
}
