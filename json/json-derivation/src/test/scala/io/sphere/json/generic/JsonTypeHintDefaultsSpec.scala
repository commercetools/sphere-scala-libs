package io.sphere.json.generic

import cats.data.Validated.Valid
import io.sphere.json._
import io.sphere.json.generic.JsonTypeHintDefaultsSpec._
import org.json4s._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonTypeHintDefaultsSpec extends AnyWordSpec with Matchers {

  "JSONTypeHintField default value" must {

    "fallback to default implementation when discriminator field is missing" in {
      val json = """{ "addressKey": "address" }"""
      getFromJSON[WithDefaultTypeField](json) must be(MissingHint("address"))
      getFromJSON[WithOverridenHintField](json) must be(MissingHint2("address"))
    }

    "test only for default implementation when discriminator field is missing" in {
      val json = """{ "shippingKey": "wrong field" }"""
      a[JSONException] must be thrownBy getFromJSON[WithDefaultTypeField](json)
      a[JSONException] must be thrownBy getFromJSON[WithOverridenHintField](json)
    }

    "ignore default type when discriminator is provided" in {
      getFromJSON[WithDefaultTypeField](
        """
          {
            "type": "MissingHint",
            "addressKey": "address"
          }
        """.stripMargin) must be(MissingHint("address"))
      getFromJSON[WithOverridenHintField](
        """
          {
            "version": "old",
            "addressKey": "address"
          }
        """.stripMargin) must be(MissingHint2("address"))
    }
  }
}

object JsonTypeHintDefaultsSpec {
  @JSONTypeHintField(value = "type", defaultType = "MissingHint")
  sealed trait WithDefaultTypeField
  case class MissingHint(addressKey: String) extends WithDefaultTypeField
  case class WithHint(shippingKey: String) extends WithDefaultTypeField
  object WithDefaultTypeField {
    implicit val json: JSON[WithDefaultTypeField] = deriveJSON[WithDefaultTypeField]
  }


  @JSONTypeHintField(value = "version", defaultType = "old")
  sealed trait WithOverridenHintField
  @JSONTypeHint("old")
  case class MissingHint2(addressKey: String) extends WithOverridenHintField
  @JSONTypeHint("new")
  case class WithHint2(shippingKey: String) extends WithOverridenHintField
  object WithOverridenHintField {
    implicit val json: JSON[WithOverridenHintField] = deriveJSON[WithOverridenHintField]
  }
}
