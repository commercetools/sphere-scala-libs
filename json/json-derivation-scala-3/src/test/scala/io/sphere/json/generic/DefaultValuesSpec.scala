package io.sphere.json.generic

import io.sphere.json._
import io.sphere.json.generic._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DefaultValuesSpec extends AnyWordSpec with Matchers {
  import DefaultValuesSpec._

  "deriving JSON" must {
    "handle default values" in {
      val json = "{  }"
      val test = getFromJSON[Test](json)
      test.value1 must be("hello")
      test.value2 must be(None)
      test.value3 must be(Some("hi"))
    }
    "handle Option with no explicit default values" in {
      val json = "{  }"
      val test2 = getFromJSON[Test2](json)
      test2.value1 must be("hello")
      test2.value2 must be (None)
    }
  }
}

object DefaultValuesSpec {
  case class Test(
      value1: String = "hello",
      value2: Option[String] = None,
      value3: Option[String] = Some("hi")
  )
  object Test {
    implicit val json: JSON[Test] = deriveJSON[Test]
  }
  case class Test2(
      value1: String = "hello",
      value2: Option[String]
  )
  object Test2 {
    implicit val json: JSON[Test2] = deriveJSON[Test2]
  }
}
