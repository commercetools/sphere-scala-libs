package io.sphere.mongo

import com.mongodb.DBObject
import org.scalatest.{MustMatchers, WordSpec}
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format.DefaultMongoFormats._

case class Something(a: Option[Int], b: Int)

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

}