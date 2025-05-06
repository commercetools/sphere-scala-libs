package io.sphere.mongo.generic

import io.sphere.json._
import io.sphere.json.generic._
import org.json4s.DefaultReaders._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JSONKeySpec extends AnyWordSpec with Matchers {
  import JSONKeySpec._

  "deriving JSON" must {
    "rename fields annotated with @JSONKey" in {
      val test =
        Test(value1 = "value1", value2 = "value2", subTest = SubTest(value2 = "other_value2"))

      val json = toJValue(test)
      (json \ "value1").as[Option[String]] must be(Some("value1"))
      (json \ "value2").as[Option[String]] must be(None)
      (json \ "new_value_2").as[Option[String]] must be(Some("value2"))
      (json \ "new_sub_value_2").as[Option[String]] must be(Some("other_value2"))

      val newTest = getFromJValue[Test](json)
      newTest must be(test)
    }
  }
}

object JSONKeySpec {
  case class SubTest(
      @JSONKey("new_sub_value_2") value2: String
  )
  object SubTest {
    implicit val mongo: JSON[SubTest] = jsonProduct(apply _)
  }

  case class Test(
      value1: String,
      @JSONKey("new_value_2") value2: String,
      @JSONEmbedded subTest: SubTest
  )
  object Test {
    implicit val mongo: JSON[Test] = jsonProduct(apply _)
  }
}
