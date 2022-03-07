/*
package io.sphere.json

import io.sphere.json.generic._
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

object JSONEmbeddedSpec {

  case class Embedded(value1: String, value2: Int)

  object Embedded {
    implicit val json: JSON[Embedded] = jsonProduct(apply _)
  }

  case class Test1(name: String, @JSONEmbedded embedded: Embedded)

  object Test1 {
    implicit val json: JSON[Test1] = jsonProduct(apply _)
  }

  case class Test2(name: String, @JSONEmbedded embedded: Option[Embedded] = None)

  object Test2 {
    implicit val json: JSON[Test2] = jsonProduct(apply _)
  }

  case class SubTest4(@JSONEmbedded embedded: Embedded)
  object SubTest4 {
    implicit val json: JSON[SubTest4] = jsonProduct(apply _)
  }

  case class Test4(subField: Option[SubTest4] = None)
  object Test4 {
    implicit val json: JSON[Test4] = jsonProduct(apply _)
  }
}

class JSONEmbeddedSpec extends AnyWordSpec with Matchers with OptionValues {
  import JSONEmbeddedSpec._

  "JSONEmbedded" should {
    "flatten the json in one object" in {
      val json =
        """{
          |  "name": "ze name",
          |  "value1": "ze value1",
          |  "value2": 45
          |}
        """.stripMargin
      val test1 = getFromJSON[Test1](json)
      test1.name mustEqual "ze name"
      test1.embedded.value1 mustEqual "ze value1"
      test1.embedded.value2 mustEqual 45

      val result = toJSON(test1)
      parseJSON(result) mustEqual parseJSON(json)
    }

    "validate that the json contains all needed fields" in {
      val json =
        """{
          |  "name": "ze name",
          |  "value1": "ze value1"
          |}
        """.stripMargin
      fromJSON[Test1](json).isInvalid must be(true)
      fromJSON[Test1]("""{"name": "a"}""").isInvalid must be(true)
    }

    "support optional embedded attribute" in {
      val json =
        """{
          |  "name": "ze name",
          |  "value1": "ze value1",
          |  "value2": 45
          |}
        """.stripMargin
      val test2 = getFromJSON[Test2](json)
      test2.name mustEqual "ze name"
      test2.embedded.value.value1 mustEqual "ze value1"
      test2.embedded.value.value2 mustEqual 45

      val result = toJSON(test2)
      parseJSON(result) mustEqual parseJSON(json)
    }

    "ignore unknown fields" in {
      val json =
        """{
          |  "name": "ze name",
          |  "value1": "ze value1",
          |  "value2": 45,
          |  "value3": true
          |}
        """.stripMargin
      val test2 = getFromJSON[Test2](json)
      test2.name mustEqual "ze name"
      test2.embedded.value.value1 mustEqual "ze value1"
      test2.embedded.value.value2 mustEqual 45
    }

    "check for sub-fields" in {
      val json =
        """
        {
          "subField": {
            "value1": "ze value1",
            "value2": 45
          }
        }
        """
      val test4 = getFromJSON[Test4](json)
      test4.subField.value.embedded.value1 mustEqual "ze value1"
      test4.subField.value.embedded.value2 mustEqual 45
    }

    "support the absence of optional embedded attribute" in {
      val json = """{ "name": "ze name" }"""
      val test2 = getFromJSON[Test2](json)
      test2.name mustEqual "ze name"
      test2.embedded mustEqual None
    }

    "validate the absence of some embedded attributes" in {
      val json = """{ "name": "ze name", "value1": "ze value1" }"""
      fromJSON[Test2](json).isInvalid must be(true)
    }
  }

}
*/
