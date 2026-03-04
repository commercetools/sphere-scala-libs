package io.sphere.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class CurrencySpec extends AnyFunSpec with Matchers {

  describe("Currency") {

    it(
      "Currency.getInstance should reject incorrect custom currencies with an IllegalArgumentException to match the java.util.Currency behaviour") {
      an[IllegalArgumentException] should be thrownBy {
        Currency.getInstance("XXX1")
      }
    }
  }

}
