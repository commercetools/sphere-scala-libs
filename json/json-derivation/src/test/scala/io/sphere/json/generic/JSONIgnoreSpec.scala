package io.sphere.json.generic

import org.json4s.JsonAST.{JInt, JObject, JString}
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

object JSONIgnoreSpec {
  case class MissingDefault(name: String, @JSONIgnore age: Int)

  case class Complete(name: String, @JSONIgnore age: Int = 100)
}

class JSONIgnoreSpec extends AnyWordSpec with Matchers with OptionValues {
  import JSONIgnoreSpec._

  "JSONIgnore" when {
    "annotated field has no default" must {
      "fail with a suitable message" in {
        val e = the[Exception] thrownBy deriveJSON[MissingDefault]
        e.getMessage mustBe "Ignored JSON field 'age' must have a default value."
      }
    }
    "annotated field has a default value" must {
      "omit the field on write and use the default on read" in {
        val json = deriveJSON[Complete]

        val written = json.write(Complete("aName"))
        written must be(JObject("name" -> JString("aName")))

        json.read(written).toOption.value must be(Complete("aName"))
      }

      "ignore the field on read even when it is present in the JSON" in {
        val json = deriveJSON[Complete]

        val input = JObject("name" -> JString("aName"), "age" -> JInt(50))
        json.read(input).toOption.value must be(Complete("aName", age = 100))
      }
    }
  }
}
