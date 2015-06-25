package io.sphere.json

import io.sphere.json.generic._
import org.scalatest.{OptionValues, MustMatchers, WordSpec}

object OptionReaderSpec {

  case class SimpleClass(value1: String, value2: Int)

  object SimpleClass {
    implicit val json: JSON[SimpleClass] = jsonProduct((apply _).curried)
  }

}


class OptionReaderSpec extends WordSpec with MustMatchers with OptionValues {
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
      fromJSON[Option[SimpleClass]](json).isFailure must be (true)
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
  }

}
