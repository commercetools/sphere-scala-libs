package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.dbObj
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SumTypesDerivingScala2Spec extends AnyWordSpec with Matchers {
  import SumTypesDerivingSpec._

  "Serializing sum types" must {

    "do not use sealed trait info when using a case class directly" in {
      check(Color8.format, Color8.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))

      check(Color8.Custom.format, Color8.Custom("2356"), dbObj("rgb" -> "2356"))

      // unless annotated

      check(
        Color8.format,
        Color8.CustomAnnotated("1111"),
        dbObj("type" -> "CustomAnnotated", "rgb" -> "1111"))

      check(
        Color8.CustomAnnotated.format,
        Color8.CustomAnnotated("2356"),
        dbObj("type" -> "CustomAnnotated", "rgb" -> "2356"))
    }

  }
}
