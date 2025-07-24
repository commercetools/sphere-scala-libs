package io.sphere.mongo.generic

import io.sphere.json._
import io.sphere.json.generic._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DefaultValuesSpec extends AnyWordSpec with Matchers {
  import DefaultValuesSpec._

  "deriving JSON" must {
    "handle default values" in {
      val json = "{}"
      val test = getFromJSON[Test](json)
      test.value1 must be("hello")
      test.value2 must be(None)
      test.value3 must be(None)
      test.value4 must be(Some("hi"))
    }
  }
}

object DefaultValuesSpec {
  case class Test(
      value1: String = "hello",
      value2: Option[String],
      value3: Option[String] = None,
      value4: Option[String] = Some("hi")
  )
  object Test {
    implicit val mongo: JSON[Test] = deriveJSON
  }
}
