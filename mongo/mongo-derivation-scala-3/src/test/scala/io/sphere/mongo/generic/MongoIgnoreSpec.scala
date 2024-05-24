package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.*
import io.sphere.mongo.generic.DefaultMongoFormats.given
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.chaining.*

object MongoIgnoreSpec {
  private val dbo = dbObj(
    "name" -> "aName"
  )

  private case class MissingDefault(name: String, @MongoIgnore age: Int)
}

class MongoIgnoreSpec extends AnyWordSpec with Matchers with OptionValues {
  import MongoIgnoreSpec.*

  "MongoIgnore" when {
    "annotated field has no default" must {
      "fail with a suitable message" in {
        val e = the[Exception] thrownBy deriveMongoFormat[MissingDefault].fromMongoValue(dbo)
        e.getMessage mustBe "Missing default parameter value for ignored field `age` on deserialization."
      }
    }
  }
}
