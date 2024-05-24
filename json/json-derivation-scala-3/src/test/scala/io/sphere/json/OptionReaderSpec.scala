package io.sphere.json

import io.sphere.json.generic._
import org.json4s.{JArray, JLong, JNothing, JObject, JString}
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.json4s.DefaultJsonFormats.given

object OptionReaderSpec {

  case class SimpleClass(value1: String, value2: Int)

  object SimpleClass {
    implicit val json: JSON[SimpleClass] = deriveJSON[SimpleClass]
  }

  case class ComplexClass(name: String, simpleClass: Option[SimpleClass])

  object ComplexClass {
    implicit val json: JSON[ComplexClass] = deriveJSON[ComplexClass]
  }

  case class MapClass(id: Long, map: Option[Map[String, String]])
  object MapClass {
    implicit val json: JSON[MapClass] = deriveJSON[MapClass]
  }

  case class ListClass(id: Long, list: Option[List[String]])
  object ListClass {
    implicit val json: JSON[ListClass] = deriveJSON[ListClass]
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
      fromJSON[Option[SimpleClass]](json).isInvalid must be(true)
    }

    "handle absence of all fields" in {
      val json = "{}"
      val result = getFromJSON[Option[SimpleClass]](json)
      result must be(None)
    }

    "handle optional map" ignore  {
      getFromJValue[MapClass](JObject("id" -> JLong(1L))) mustEqual MapClass(1L, None)

      getFromJValue[MapClass](JObject("id" -> JLong(1L), "map" -> JObject())) mustEqual
        MapClass(1L, Some(Map.empty))

      getFromJValue[MapClass](
        JObject("id" -> JLong(1L), "map" -> JObject("a" -> JString("b")))) mustEqual
        MapClass(1L, Some(Map("a" -> "b")))

      toJValue[MapClass](MapClass(1L, None)) mustEqual
        JObject("id" -> JLong(1L), "map" -> JNothing)
      toJValue[MapClass](MapClass(1L, Some(Map()))) mustEqual
        JObject("id" -> JLong(1L), "map" -> JObject())
      toJValue[MapClass](MapClass(1L, Some(Map("a" -> "b")))) mustEqual
        JObject("id" -> JLong(1L), "map" -> JObject("a" -> JString("b")))
    }

    "handle optional list" ignore  {
      getFromJValue[ListClass](
        JObject("id" -> JLong(1L), "list" -> JArray(List(JString("hi"))))) mustEqual
        ListClass(1L, Some(List("hi")))
      getFromJValue[ListClass](JObject("id" -> JLong(1L), "list" -> JArray(List.empty))) mustEqual
        ListClass(1L, Some(List()))
      getFromJValue[ListClass](JObject("id" -> JLong(1L))) mustEqual
        ListClass(1L, None)

      toJValue(ListClass(1L, Some(List("hi")))) mustEqual JObject(
        "id" -> JLong(1L),
        "list" -> JArray(List(JString("hi"))))
      toJValue(ListClass(1L, Some(List.empty))) mustEqual JObject(
        "id" -> JLong(1L),
        "list" -> JArray(List.empty))
      toJValue(ListClass(1L, None)) mustEqual JObject("id" -> JLong(1L), "list" -> JNothing)
    }

    "handle absence of all fields mixed with ignored fields" ignore {
      val json = """{ "value3": "a" }"""
      val result = getFromJSON[Option[SimpleClass]](json)
      result must be(None)
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
