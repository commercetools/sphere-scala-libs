package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils._
import io.sphere.mongo.generic.deriveMongoFormat
import io.sphere.mongo.format.DefaultMongoFormats._
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

object MongoIgnoreSpec {
  private val aName = "aName"
  private val dbo = dbObj("name" -> aName)

  private case class MissingDefault(name: String, @MongoIgnore age: Int)

  private val defaultAge = 100
  private case class Complete(name: String, @MongoIgnore age: Int = defaultAge)
}

class MongoIgnoreSpec extends AnyWordSpec with Matchers with OptionValues {
  import MongoIgnoreSpec._

  "MongoIgnore" when {
    "annotated field has no default" must {
      "fail with a suitable message" in {
        val e = the[Exception] thrownBy deriveMongoFormat[MissingDefault].fromMongoValue(dbo)
        e.getMessage mustBe "Ignored Mongo field 'age' must have a default value."
      }
    }
    "annotated field has also a default" must {
      "omit the field in serialization" in {
        deriveMongoFormat[Complete].toMongoValue(Complete(aName)) mustBe dbo
      }
    }
  }
}
