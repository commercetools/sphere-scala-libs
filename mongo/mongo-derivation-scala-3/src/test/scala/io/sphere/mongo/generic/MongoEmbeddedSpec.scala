package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.*
import io.sphere.mongo.generic.DefaultMongoFormats.given
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

object MongoEmbeddedSpec {
  case class Embedded(value1: String, @MongoKey("_value2") value2: Int)

  case class Test1(name: String, @MongoEmbedded embedded: Embedded)

  case class Test2(name: String, @MongoEmbedded embedded: Option[Embedded] = None)

  case class Test3(
      @MongoIgnore name: String = "default",
      @MongoEmbedded embedded: Option[Embedded] = None)

  case class SubTest4(@MongoEmbedded embedded: Embedded)

  case class Test4(subField: Option[SubTest4] = None)
}

class MongoEmbeddedSpec extends AnyWordSpec with Matchers with OptionValues {
  import MongoEmbeddedSpec.*

  "MongoEmbedded" should {
    "flatten the db object in one object" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1",
        "_value2" -> 45
      )
      val test1 = deriveMongoFormat[Test1].fromMongoValue(dbo)
      test1.name mustEqual "ze name"
      test1.embedded.value1 mustEqual "ze value1"
      test1.embedded.value2 mustEqual 45

      val result = deriveMongoFormat[Test1].toMongoValue(test1)
      result mustEqual dbo
    }

    "validate that the db object contains all needed fields" in {
      // TODO default field
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1"
      )
      Try(deriveMongoFormat[Test1].fromMongoValue(dbo)).isFailure must be(true)
    }

    "support optional embedded attribute" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1",
        "_value2" -> 45
      )
      val test2 = deriveMongoFormat[Test2].fromMongoValue(dbo)
      test2.name mustEqual "ze name"
      test2.embedded.value.value1 mustEqual "ze value1"
      test2.embedded.value.value2 mustEqual 45

      val result = deriveMongoFormat[Test2].toMongoValue(test2)
      result mustEqual dbo
    }

    "ignore unknown fields" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1",
        "_value2" -> 45,
        "value4" -> true
      )
      val test2 = deriveMongoFormat[Test2].fromMongoValue(dbo)
      test2.name mustEqual "ze name"
      test2.embedded.value.value1 mustEqual "ze value1"
      test2.embedded.value.value2 mustEqual 45
    }

//    "ignore ignored fields" in pendingUntilFixed {
//      // TODO Ignore
//      val dbo = dbObj(
//        "value1" -> "ze value1",
//        "_value2" -> 45
//      )
//      val test3 = deriveMongoFormat[Test3].fromMongoValue(dbo)
//      test3.name mustEqual "default"
//      test3.embedded.value.value1 mustEqual "ze value1"
//      test3.embedded.value.value2 mustEqual 45
//    }

    "check for sub-fields" in {
      val dbo = dbObj(
        "subField" -> dbObj(
          "value1" -> "ze value1",
          "_value2" -> 45
        )
      )
      val test4 = deriveMongoFormat[Test4].fromMongoValue(dbo)
      test4.subField.value.embedded.value1 mustEqual "ze value1"
      test4.subField.value.embedded.value2 mustEqual 45
    }

    "support the absence of optional embedded attribute" in {
      val dbo = dbObj(
        "name" -> "ze name"
      )
      val test2 = deriveMongoFormat[Test2].fromMongoValue(dbo)
      test2.name mustEqual "ze name"
      test2.embedded mustEqual None
    }

    "validate the absence of some embedded attributes" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1"
      )
      Try(deriveMongoFormat[Test2].fromMongoValue(dbo)).isFailure must be(true)
    }
  }
}
