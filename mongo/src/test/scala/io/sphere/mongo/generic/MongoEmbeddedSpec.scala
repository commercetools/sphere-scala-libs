package io.sphere.mongo.generic

import io.sphere.mongo.format.MongoFormat
import org.scalatest.{MustMatchers, OptionValues, WordSpec}
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.MongoUtils._

import scala.util.Try

object MongoEmbeddedSpec {
  case class Embedded(value1: String, value2: Int)

  object Embedded {
    implicit val mongo: MongoFormat[Embedded] = mongoProduct(apply _)
  }

  case class Test1(
    name: String,
    @MongoEmbedded embedded: Embedded)

  object Test1 {
    implicit val mongo: MongoFormat[Test1] = mongoProduct(apply _)
  }

  case class Test2(
    name: String,
    @MongoEmbedded embedded: Option[Embedded] = None)

  object Test2 {
    implicit val mongo: MongoFormat[Test2] = mongoProduct(apply _)
  }
}

class MongoEmbeddedSpec extends WordSpec with MustMatchers with OptionValues {
  import MongoEmbeddedSpec._

  "MongoEmbedded" should {
    "flatten the db object in one object" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1",
        "value2" -> 45
      )
      val test1 = MongoFormat[Test1].fromMongoValue(dbo)
      test1.name mustEqual "ze name"
      test1.embedded.value1 mustEqual "ze value1"
      test1.embedded.value2 mustEqual 45

      val result = MongoFormat[Test1].toMongoValue(test1)
      result mustEqual dbo
    }

    "validate that the db object contains all needed fields" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1"
      )
      Try(MongoFormat[Test1].fromMongoValue(dbo)).isFailure must be (true)
    }

    "support optional embedded attribute" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1",
        "value2" -> 45
      )
      val test2 = MongoFormat[Test2].fromMongoValue(dbo)
      test2.name mustEqual "ze name"
      test2.embedded.value.value1 mustEqual "ze value1"
      test2.embedded.value.value2 mustEqual 45

      val result = MongoFormat[Test2].toMongoValue(test2)
      result mustEqual dbo
    }

    "ignore unknown fields" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1",
        "value2" -> 45,
        "value3" -> true
      )
      val test2 = MongoFormat[Test2].fromMongoValue(dbo)
      test2.name mustEqual "ze name"
      test2.embedded.value.value1 mustEqual "ze value1"
      test2.embedded.value.value2 mustEqual 45
    }

    "support the absence of optional embedded attribute" in {
      val dbo = dbObj(
        "name" -> "ze name"
      )
      val json = """{ "name": "ze name" }"""
      val test2 = MongoFormat[Test2].fromMongoValue(dbo)
      test2.name mustEqual "ze name"
      test2.embedded mustEqual None
    }

    "validate the absence of some embedded attributes" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1"
      )
      Try(MongoFormat[Test2].fromMongoValue(dbo)).isFailure must be (true)
    }
  }
}
