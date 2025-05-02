package io.sphere.mongo.generic

import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.format.MongoFormat
import org.bson.BSONObject
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class MongoKeySpec extends AnyWordSpec with Matchers {
  import MongoKeySpec._

  "deriving MongoFormat" must {
    "rename fields annotated with @MongoKey" in {
      val test =
        Test(value1 = "value1", value2 = "value2", subTest = SubTest(value2 = "other_value2"))

      val dbo = MongoFormat[Test].toMongoValue(test)
      val map = dbo.asInstanceOf[BSONObject].toMap.asScala.toMap[Any, Any]
      map.get("value1") must equal(Some("value1"))
      map.get("value2") must equal(None)
      map.get("new_value_2") must equal(Some("value2"))
      map.get("new_sub_value_2") must equal(Some("other_value2"))

      val newTest = MongoFormat[Test].fromMongoValue(dbo)
      newTest must be(test)
    }
  }
}

object MongoKeySpec {
  case class SubTest(
      @MongoKey("new_sub_value_2") value2: String
  )
  object SubTest {
    implicit val mongo: MongoFormat[SubTest] = mongoProduct(apply _)
  }

  case class Test(
      value1: String,
      @MongoKey("new_value_2") value2: String,
      @MongoEmbedded subTest: SubTest
  )
  object Test {
    implicit val mongo: MongoFormat[Test] = mongoProduct(apply _)
  }
}
