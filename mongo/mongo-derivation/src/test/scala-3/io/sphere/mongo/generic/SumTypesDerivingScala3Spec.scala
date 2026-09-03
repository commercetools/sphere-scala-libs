package io.sphere.mongo.generic

import com.mongodb.DBObject
import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.generic.deriveMongoFormat
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format.DefaultMongoFormats.given
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SumTypesDerivingScala3Spec extends AnyWordSpec with Matchers {
  import SumTypesDerivingScala3Spec.*

  "Serializing sum types" must {

    "The typeDiscriminator should be handle by the trait" in {
      check(Color8.format, Color8.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))

      check(Color8.Custom.format, Color8.Custom("2356"), dbObj("rgb" -> "2356"))

      // unless annotated

      check(
        Color8.format,
        Color8.CustomAnnotated("1111"),
        dbObj("type" -> "CustomAnnotated", "rgb" -> "1111"))

      // I decided to drop adding the type field to case classes directly.
      // It's not used and the trait should handle it
      check(Color8.CustomAnnotated.format, Color8.CustomAnnotated("2356"), dbObj("rgb" -> "2356"))
    }

  }
}

object SumTypesDerivingScala3Spec {
  import Matchers.*

  def check[A, B <: A](format: MongoFormat[A], b: B, dbo: DBObject): Assertion = {
    val serialized = format.toMongoValue(b)
    serialized must be(dbo)

    format.fromMongoValue(serialized) must be(b)
  }

  sealed trait Color8
  object Color8 {
    // the formats must use `lazy` to make this code compile

    case object Red extends Color8
    case class Custom(rgb: String) extends Color8
    object Custom {
      lazy val format = deriveMongoFormat[Custom]
    }
    @MongoTypeHintField("type")
    case class CustomAnnotated(rgb: String) extends Color8
    object CustomAnnotated {
      lazy val format = deriveMongoFormat[CustomAnnotated]
    }
    lazy val format = deriveMongoFormat[Color8]
  }
}
