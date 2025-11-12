package io.sphere.mongo

import com.mongodb.{BasicDBObject, DBObject}
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.format.MongoFormat
import org.scalatest.matchers.must.Matchers
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
      something must be(Something(Some(3), 4))
    }

    "generate a format that serializes optional fields with value None as BSON objects without that field" in {
      val testFormat: MongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat
      val serializedObject = testFormat.toMongoValue(Something(None, 1)).asInstanceOf[DBObject]
      serializedObject.keySet().contains("b") must be(true)
      serializedObject.keySet().contains("a") must be(false)
    }

    "generate a format that use default values" in {
      implicit val mongoFormat: MongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat

      val sthObj1 = {
        val dbo = new BasicDBObject()
        dbo.put("a", Integer.valueOf(3))
        dbo
      }
      val s1 = MongoFormat[Something].fromMongoValue(sthObj1)
      s1 must be(Something(a = Some(3), b = 2))

      val sthObj2 = new BasicDBObject() // an empty object
      val s2 = MongoFormat[Something].fromMongoValue(sthObj2)
      s2 must be(Something(a = None, b = 2))

      val sthObj3 = {
        val dbo = new BasicDBObject()
        dbo.put("b", Integer.valueOf(33))
        dbo
      }
      val s3 = MongoFormat[Something].fromMongoValue(sthObj3)
      s3 must be(Something(a = None, b = 33))

      val sthObj4 = {
        val dbo = new BasicDBObject()
        dbo.put("a", Integer.valueOf(33))
        dbo.put("b", Integer.valueOf(44))
        dbo
      }
      val s4 = MongoFormat[Something].fromMongoValue(sthObj4)
      s4 must be(Something(a = Some(33), b = 44))

    }
  }

  "mongoEnum" must {
    "serialize and deserialize enumerations" in {
      val mongo: MongoFormat[Color.Value] = generic.mongoEnum(Color)

      val colors = List(Color.Red, Color.Yellow, Color.Blue)
      val roundTripColors = colors.map(mongo.toMongoValue).map(mongo.fromMongoValue)
      colors must be(roundTripColors)

      // mongo java driver knows how to encode/decode Strings
      val serializedObject = mongo.toMongoValue(Color.Red).asInstanceOf[String]
      serializedObject must be("Red")

      val enumValue = mongo.fromMongoValue(serializedObject)
      enumValue must be(Color.Red)
    }
  }
}
