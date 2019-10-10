package io.sphere.mongo.generic

import io.sphere.json._
import io.sphere.json.generic._
import io.sphere.json.generic.derive.MagnoliaJSONDerivation
import org.json4s.DefaultReaders._
import org.scalatest.{MustMatchers, WordSpec}

class JSONKey2Spec extends WordSpec with MustMatchers {
  import JSONKey2Spec._

  "deriving JSON" must {
    "rename fields annotated with @JSONKey" in {
      val test = Test(
        value1 = "value1",
        value2 = "value2",
        subTest = SubTest(
          value2 = "other_value2"))

      val json = toJValue(test)
      (json \ "value1").as[Option[String]] must be (Some("value1"))
      (json \ "value2").as[Option[String]] must be (None)
      (json \ "new_value_2").as[Option[String]] must be (Some("value2"))
      (json \ "new_sub_value_2").as[Option[String]] must be (Some("other_value2"))

      val newTest = getFromJValue[Test](json)
      newTest must be (test)
    }
  }
}

object JSONKey2Spec {
  case class SubTest(
    @JSONKey2("new_sub_value_2") value2: String
  )
  object SubTest {
    implicit val mongo: JSON[SubTest] = MagnoliaJSONDerivation.jsonProduct
  }

  case class Test(
    value1: String,
    @JSONKey2("new_value_2") value2: String,
    @JSONEmbedded2 subTest: SubTest
  )
  object Test {
    implicit val mongo: JSON[Test] = MagnoliaJSONDerivation.jsonProduct
  }
}
