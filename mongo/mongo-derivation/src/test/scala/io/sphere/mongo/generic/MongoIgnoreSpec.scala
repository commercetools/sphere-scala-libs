package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils._
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.generic.deriveMongoFormat
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

object MongoIgnoreSpec {
  private case class MissingDefault(name: String, @MongoIgnore age: Int)

  private case class Complete(name: String, @MongoIgnore age: Int = 100)
}

class MongoIgnoreSpec extends AnyWordSpec with Matchers with OptionValues {
  import MongoIgnoreSpec._

  "MongoIgnore" when {
    "annotated field has no default" must {
      "fail with a suitable message" in {
        val e = the[Exception] thrownBy deriveMongoFormat[MissingDefault]
        e.getMessage mustBe "Ignored Mongo field 'age' must have a default value."
      }
    }
    "annotated field has a default value" must {
      "omit the field from the BSON" in {
        val aName = "aName"
        val format = deriveMongoFormat[Complete]
        format.toMongoValue(Complete(aName)) mustBe dbObj("name" -> aName)
      }
    }
  }
}
