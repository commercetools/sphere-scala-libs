package io.sphere.mongo

import com.mongodb.{BasicDBObject, DBObject}
import org.scalatest.matchers.must.Matchers
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format.DefaultMongoFormats._
import org.scalatest.wordspec.AnyWordSpec

object SerializationTest {
  case class Something(a: Option[Int], b: Int = 2)

  object Color extends Enumeration {
    val Blue, Red, Yellow = Value
  }
}

class SerializationTest extends AnyWordSpec with Matchers {
  import SerializationTest._

  "mongoProduct" must {
    "deserialize mongo object" in {
      val dbo = new BasicDBObject()
      dbo.put("a", Integer.valueOf(3))
      dbo.put("b", Integer.valueOf(4))

      val mongoFormat: MongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat
      val something = mongoFormat.fromMongoValue(dbo)
      something must be (Something(Some(3), 4))
    }

    "generate a format that serializes optional fields with value None as BSON objects without that field" in {
      val testFormat: MongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat[Something]
      val serializedObject = testFormat.toMongoValue(Something(None, 1)).asInstanceOf[DBObject]
      serializedObject.keySet().contains("b") must be(true)
      serializedObject.keySet().contains("a") must be(false)
    }

    "generate a format that use default values" in {
      val dbo = new BasicDBObject()
      dbo.put("a", Integer.valueOf(3))

      val mongoFormat: MongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat
      val something = mongoFormat.fromMongoValue(dbo)
      something must be (Something(Some(3), 2))
    }
  }

  "mongoEnum" must {
    "serialize and deserialize enums" in {
      val mongo: MongoFormat[Color.Value] = generic.mongoEnum(Color)

      // mongo java driver knows how to encode/decode Strings
      val serializedObject = mongo.toMongoValue(Color.Red).asInstanceOf[String]
      serializedObject must be ("Red")

      val enumValue = mongo.fromMongoValue(serializedObject)
      enumValue must be (Color.Red)
    }
  }

}
