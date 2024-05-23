package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.*
import io.sphere.mongo.generic.DefaultMongoFormats.given
import io.sphere.mongo.format.MongoFormat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DefaultValuesSpec extends AnyWordSpec with Matchers:
  import DefaultValuesSpec.*

  "deriving MongoFormat" must {
    "handle default values" in {
      val dbo = dbObj()
      val test = TypedMongoFormat[Test].fromMongoValue(dbo)
      test.value1 mustBe "hello"
      test.value2 mustBe None
      test.value3 mustBe None
      test.value4 mustBe Some("hi")
    }
  }

object DefaultValuesSpec:
  case class Test(
      value1: String = "hello",
      value2: Option[String],
      value3: Option[String] = None,
      value4: Option[String] = Some("hi")
  )
  object Test:
    implicit val mongo: TypedMongoFormat[Test] = deriveMongoFormat
