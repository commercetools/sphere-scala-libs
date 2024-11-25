package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.*
import io.sphere.mongo.format.DefaultMongoFormats.given
import io.sphere.mongo.format.{MongoFormat, deriveMongoFormat}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DefaultValuesSpec extends AnyWordSpec with Matchers {

  import DefaultValuesSpec.*

  "deriving TypedMongoFormat" must {
    "handle default values" in {
      val dbo = dbObj()
      val test = MongoFormat[CaseClass].fromMongoValue(dbo)
      import test._
      field1 mustBe "hello"
      field2 mustBe None
      field3 mustBe None
      field4 mustBe Some("hi")
    }
  }
}

object DefaultValuesSpec {
  case class CaseClass(
      field1: String = "hello",
      field2: Option[String],
      field3: Option[String] = None,
      field4: Option[String] = Some("hi")
  )

  object CaseClass {
    given MongoFormat[CaseClass] = deriveMongoFormat[CaseClass]
  }
}
