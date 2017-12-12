package io.sphere.mongo

import com.mongodb.DBObject
import org.scalatest.{MustMatchers, WordSpec}
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format.DefaultMongoFormats._

case class Something(a: Option[Int], b: Int)

object Color extends Enumeration {
  val Blue, Red, Yellow = Value
}

class SerializationTest extends WordSpec with MustMatchers {

  "mongoProduct" must {
    "generate a format that serializes optional fields with value None as BSON objects without that field" in {
      val testFormat: MongoFormat[Something] =
        io.sphere.mongo.generic.mongoProduct[Something, Option[Int], Int] {
          (a: Option[Int]) => (b: Int) => Something(a, b)
        }

      val serializedObject = testFormat.toMongoValue(Something(None, 1)).asInstanceOf[DBObject]
      serializedObject.keySet().contains("b") must be(true)
      serializedObject.keySet().contains("a") must be(false)

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
