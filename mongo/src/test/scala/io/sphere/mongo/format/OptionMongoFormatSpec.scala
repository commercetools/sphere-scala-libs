package io.sphere.mongo.format

import io.sphere.mongo.generic._
import org.scalatest.{MustMatchers, OptionValues, WordSpec}
import io.sphere.mongo.MongoUtils._
import DefaultMongoFormats._

object OptionMongoFormatSpec {

  case class SimpleClass(value1: String, value2: Int)

  object SimpleClass {
    implicit val mongo: MongoFormat[SimpleClass] = mongoProduct
  }

  case class ComplexClass(
    name: String,
    simpleClass: Option[SimpleClass])

  object ComplexClass {
    implicit val mongo: MongoFormat[ComplexClass] = mongoProduct
  }

}


class OptionMongoFormatSpec extends WordSpec with MustMatchers with OptionValues {
  import OptionMongoFormatSpec._

  "MongoFormat[Option[_]]" should {
    "handle presence of all fields" in {
      val dbo = dbObj(
        "value1" -> "a",
        "value2" -> 45
      )
      val result = MongoFormat[Option[SimpleClass]].fromMongoValue(dbo)
      result.value.value1 mustEqual "a"
      result.value.value2 mustEqual 45
    }

    "handle presence of all fields mixed with ignored fields" in {
      val dbo = dbObj(
        "value1" -> "a",
        "value2" -> 45,
        "value3" -> "b"
      )
      val result = MongoFormat[Option[SimpleClass]].fromMongoValue(dbo)
      result.value.value1 mustEqual "a"
      result.value.value2 mustEqual 45
    }

    "handle presence of not all the fields" in {
      val dbo = dbObj("value1" -> "a")
      an[Exception] mustBe thrownBy (MongoFormat[Option[SimpleClass]].fromMongoValue(dbo))
    }

    "handle absence of all fields" in {
      val dbo = dbObj()
      val result = MongoFormat[Option[SimpleClass]].fromMongoValue(dbo)
      result mustEqual None
    }

    "handle absence of all fields mixed with ignored fields" in {
      val dbo = dbObj("value3" -> "a")
      val result = MongoFormat[Option[SimpleClass]].fromMongoValue(dbo)
      result mustEqual None
    }

    "consider all fields if the data type does not impose any restriction" in {
      val dbo = dbObj(
        "key1" -> "value1",
        "key2" -> "value2"
      )
      val expected = Map("key1" -> "value1", "key2" -> "value2")
      val result = MongoFormat[Map[String, String]].fromMongoValue(dbo)
      result mustEqual expected

      val maybeResult = MongoFormat[Option[Map[String, String]]].fromMongoValue(dbo)
      maybeResult.value mustEqual expected
    }

    "parse optional element" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "simpleClass" -> dbObj(
          "value1" -> "value1",
          "value2" -> 42
        )
      )
      val result = MongoFormat[ComplexClass].fromMongoValue(dbo)
      result.simpleClass.value.value1 mustEqual "value1"
      result.simpleClass.value.value2 mustEqual 42

      MongoFormat[ComplexClass].toMongoValue(result) mustEqual dbo
    }
  }
}
