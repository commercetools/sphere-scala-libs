package io.sphere.util

import scala.language.postfixOps

import org.scalatest.FunSpec
import org.scalatest.MustMatchers

class MoneySpec extends FunSpec with MustMatchers {
  import Money._

  implicit val mode = BigDecimal.RoundingMode.UNNECESSARY

  describe("Money") {
    it("should have value semantics.") {
      (1.23 EUR) must equal (1.23 EUR)
    }

    it("should default to HALF_EVEN rounding mode when using monetary notation.") {
      (1.001 EUR) must equal (1.00 EUR)
      (1.005 EUR) must equal (1.00 EUR)
      (1.015 EUR) must equal (1.02 EUR)
      ((1.00 EUR) + 0.001) must equal (1.00 EUR)
      ((1.00 EUR) + 0.005) must equal (1.00 EUR)
      ((1.00 EUR) + 0.015) must equal (1.02 EUR)
      ((1.00 EUR) - 0.005) must equal (1.00 EUR)
      ((1.00 EUR) - 0.015) must equal (0.98 EUR)
      ((1.00 EUR) + 0.0115) must equal (1.01 EUR)
    }

    it("should not accept an amount with an invalid scale for the used currency when using the constructor directly.") {
      an [IllegalArgumentException] must be thrownBy { 
        Money(1.0001, java.util.Currency.getInstance("EUR"))
      }
    }

    it("should not be prone to common rounding errors known from floating point numbers.") {
      var m = 0.00 EUR

      for (i <- 1 to 10) m = m + 0.10

      m must equal (1.00 EUR)
    }

    it("should support the unary '-' operator.") {
      -EUR(1.00) must equal (-1.00 EUR)
    }

    it("should support the binary '+' operator.") {
      (1.42 EUR) + (1.58 EUR) must equal (3.00 EUR)
    }

    it("should support the binary '+' operator on different currencies.") {
      an [IllegalArgumentException] must be thrownBy {
        (1.42 EUR) + (1.58 USD)
      }
    }

    it("should support the binary '-' operator.") {
      (1.33 EUR) - (0.33 EUR) must equal (1.00 EUR)
    }

    it("should support the binary '*' operator, requiring a rounding mode.") {
      implicit val mode = BigDecimal.RoundingMode.HALF_EVEN
      (1.33 EUR) * (1.33 EUR) must equal (1.77 EUR)
    }

    it("should support the binary '/%' (divideAndRemainder) operator.") {
      implicit val mode = BigDecimal.RoundingMode.HALF_EVEN
      (1.33 EUR) /% 0.3 must equal (4.00 EUR, 0.13 EUR)
      (1.33 EUR) /% 0.003 must equal (443.00 EUR, 0.00 EUR)
    }

    it("should support getting the remainder of a division ('%').") {
      implicit val mode = BigDecimal.RoundingMode.HALF_EVEN
      (1.25 EUR) remainder (1.1) must equal (0.15 EUR)
      (1.25 EUR) % 1.1 must equal (0.15 EUR)
    }

    it("should support partitioning an amount without losing or gaining money.") {
      (0.05 EUR) partition (3,7) must equal (Seq(0.02 EUR, 0.03 EUR))
      (10 EUR) partition (1,2) must equal (Seq(3.34 EUR, 6.66 EUR))
      (10 EUR) partition (3,1,3) must equal (Seq(4.29 EUR, 1.43 EUR, 4.28 EUR))
    }

    it("should allow comparing money with the same currency.") {
      ((1.10 EUR) > (1.00 EUR)) must be (true)
      ((1.00 EUR) >= (1.00 EUR)) must be (true)
      ((1.00 EUR) < (1.10 EUR)) must be (true)
      ((1.00 EUR) <= (1.00 EUR)) must be (true)
    }

    it("should support currencies with a scale of 0 (i.e. Japanese Yen)") {
      (1 JPY) must equal (1 JPY)
    }

    it("should be able to update the centAmount") {
      (1.10 EUR).withCentAmount(170) must be (1.70 EUR)
      (1.10 EUR).withCentAmount(1711) must be (17.11 EUR)
      (1 JPY).withCentAmount(34) must be (34 JPY)
    }
  }
}