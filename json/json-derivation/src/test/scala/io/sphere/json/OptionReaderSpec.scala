package io.sphere.json

import io.sphere.json.generic._
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

object OptionReaderSpec {

  case class SimpleClass(value1: String, value2: Int)

  object SimpleClass {
    implicit val json: JSON[SimpleClass] = jsonProduct(apply _)
  }

  case class ComplexClass(
    name: String,
    simpleClass: Option[SimpleClass])

  object ComplexClass {
    implicit val json: JSON[ComplexClass] = jsonProduct(apply _)
  }

}


class OptionReaderSpec extends AnyWordSpec with Matchers with OptionValues {
  import OptionReaderSpec._

  "OptionReader" should {
    "handle presence of all fields" in {
      val json =
        """{
          |  "value1": "a",
          |  "value2": 45
          |}
        """.stripMargin
      val result = getFromJSON[Option[SimpleClass]](json)
      result.value.value1 mustEqual "a"
      result.value.value2 mustEqual 45
    }

    "handle presence of all fields mixed with ignored fields" in {
      val json =
        """{
          |  "value1": "a",
          |  "value2": 45,
          |  "value3": "b"
          |}
        """.stripMargin
      val result = getFromJSON[Option[SimpleClass]](json)
      result.value.value1 mustEqual "a"
      result.value.value2 mustEqual 45
    }

    "handle presence of not all the fields" in {
      val json = """{ "value1": "a" }"""
      fromJSON[Option[SimpleClass]](json).isInvalid must be (true)
    }

    "handle absence of all fields" in {
      val json = "{}"
      val result = getFromJSON[Option[SimpleClass]](json)
      result mustEqual None
    }

    "handle absence of all fields mixed with ignored fields" in {
      val json = """{ "value3": "a" }"""
      val result = getFromJSON[Option[SimpleClass]](json)
      result mustEqual None
    }

    "consider all fields if the data type does not impose any restriction" in {
      val json =
        """{
          |  "key1": "value1",
          |  "key2": "value2"
          |}
        """.stripMargin
      val expected = Map("key1" -> "value1", "key2" -> "value2")
      val result = getFromJSON[Map[String, String]](json)
      result mustEqual expected

      val maybeResult = getFromJSON[Option[Map[String, String]]](json)
      maybeResult.value mustEqual expected
    }

    "parse optional element" in {
      val json =
        """{
          |  "name": "ze name",
          |  "simpleClass": {
          |    "value1": "value1",
          |    "value2": 42
          |  }
          |}
        """.stripMargin
      val result = getFromJSON[ComplexClass](json)
      result.simpleClass.value.value1 mustEqual "value1"
      result.simpleClass.value.value2 mustEqual 42

      parseJSON(toJSON(result)) mustEqual parseJSON(json)
    }
  }

}
