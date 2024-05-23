package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters.*

class MongoKeySpec extends AnyWordSpec with Matchers {
  import MongoKeySpec.*

  "deriving MongoFormat" must {
    "rename fields annotated with @MongoKey" in {
      val test =
        Test(value1 = "value1", value2 = "value2", subTest = SubTest(value2 = "other_value2"))

      val formatter = deriveMongoFormat[Test]
      val dbo = formatter.toMongoValue(test)
      val map = dbo.asInstanceOf[BasicDBObject].toMap.asScala.toMap[Any, Any]

      map.get("value1") must equal(Some("value1"))
      map.get("value2") must equal(None)
      map.get("new_value_2") must equal(Some("value2"))
      map.get("new_sub_value_2") must equal(Some("other_value2"))

      val newTest = formatter.fromMongoValue(dbo)
      newTest must be(test)
    }
  }
}

object MongoKeySpec {
  case class SubTest(
      @MongoKey("new_sub_value_2") value2: String
  )

  case class Test(
      value1: String,
      @MongoKey("new_value_2") value2: String,
      @MongoEmbedded subTest: SubTest
  )
}
