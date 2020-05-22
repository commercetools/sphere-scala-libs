package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils._
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.format.MongoFormat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DefaultValuesSpec extends AnyWordSpec with Matchers {
  import DefaultValuesSpec._

  "deriving MongoFormat" must {
    "handle default values" in {
      val dbo = dbObj()
      val test = MongoFormat[Test].fromMongoValue(dbo)
      test.value1 must be ("hello")
      test.value2 must be (None)
      test.value3 must be (None)
      test.value4 must be (Some("hi"))
    }
  }
}

object DefaultValuesSpec {
  case class Test(
    value1: String = "hello",
    value2: Option[String],
    value3: Option[String] = None,
    value4: Option[String] = Some("hi")
  )
  object Test {
    implicit val mongo: MongoFormat[Test] = deriveMongoFormat
  }
}
