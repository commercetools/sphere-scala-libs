package io.sphere.mongo.generic

import io.sphere.mongo.format.MongoFormat
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.MongoUtils._
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

object MongoEmbeddedSpec {
  case class Embedded(value1: String, @MongoKey("_value2") value2: Int)

  object Embedded {
    implicit val mongo: MongoFormat[Embedded] = mongoProduct(apply _)
  }

  case class Test1(name: String, @MongoEmbedded embedded: Embedded)

  object Test1 {
    implicit val mongo: MongoFormat[Test1] = mongoProduct(apply _)
  }

  case class Test2(name: String, @MongoEmbedded embedded: Option[Embedded] = None)

  object Test2 {
    implicit val mongo: MongoFormat[Test2] = mongoProduct(apply _)
  }

  case class Test3(
      @MongoIgnore name: String = "default",
      @MongoEmbedded embedded: Option[Embedded] = None)

  object Test3 {
    implicit val mongo: MongoFormat[Test3] = mongoProduct(apply _)
  }

  case class SubTest4(@MongoEmbedded embedded: Embedded)
  object SubTest4 {
    implicit val mongo: MongoFormat[SubTest4] = mongoProduct(apply _)
  }

  case class Test4(subField: Option[SubTest4] = None)
  object Test4 {
    implicit val mongo: MongoFormat[Test4] = mongoProduct(apply _)
  }
 

  @MongoTypeHintField("myType")
  sealed trait SumType
  object SumType{
  @MongoTypeHint("variation1") case object Variation1 extends SumType
  @MongoTypeHint("variation2") case class Variation2(name: String) extends SumType
    implicit val mongoSumType: MongoFormat[SumType] = deriveMongoFormat[SumType]
  }
  case class Test5(@MongoEmbedded myType: SumType)
  object Test5 {
    implicit val mongo: MongoFormat[Test5] = deriveMongoFormat[Test5]
  }
}

class MongoEmbeddedSpec extends AnyWordSpec with Matchers with OptionValues {
  import MongoEmbeddedSpec._

  "MongoEmbedded" should {
    "flatten the db object in one object" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1",
        "_value2" -> 45
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
      Try(MongoFormat[Test1].fromMongoValue(dbo)).isFailure must be(true)
    }

    "support optional embedded attribute" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1",
        "_value2" -> 45
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
        "_value2" -> 45,
        "value4" -> true
      )
      val test2 = MongoFormat[Test2].fromMongoValue(dbo)
      test2.name mustEqual "ze name"
      test2.embedded.value.value1 mustEqual "ze value1"
      test2.embedded.value.value2 mustEqual 45
    }

    "ignore ignored fields" in {
      val dbo = dbObj(
        "value1" -> "ze value1",
        "_value2" -> 45
      )
      val test3 = MongoFormat[Test3].fromMongoValue(dbo)
      test3.name mustEqual "default"
      test3.embedded.value.value1 mustEqual "ze value1"
      test3.embedded.value.value2 mustEqual 45
    }

    "check for sub-fields" in {
      val dbo = dbObj(
        "subField" -> dbObj(
          "value1" -> "ze value1",
          "_value2" -> 45
        )
      )
      val test4 = MongoFormat[Test4].fromMongoValue(dbo)
      test4.subField.value.embedded.value1 mustEqual "ze value1"
      test4.subField.value.embedded.value2 mustEqual 45
    }

    "support the absence of optional embedded attribute" in {
      val dbo = dbObj(
        "name" -> "ze name"
      )
      val test2 = MongoFormat[Test2].fromMongoValue(dbo)
      test2.name mustEqual "ze name"
      test2.embedded mustEqual None
    }

    "validate the absence of some embedded attributes" in {
      val dbo = dbObj(
        "name" -> "ze name",
        "value1" -> "ze value1"
      )
      Try(MongoFormat[Test2].fromMongoValue(dbo)).isFailure must be(true)
    }
    "embed sum types with expected type hint fields" in {
      val dbo = dbObj(
        "myType" -> "variation1"
      )
      val test5 = MongoFormat[SumType].fromMongoValue(dbo)
      test5 mustEqual  SumType.Variation1

    }
  }
}
