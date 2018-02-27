import java.util.Currency

import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import org.scalatest._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class HighPrecisionMoneySpec extends FunSpec with MustMatchers {
  import HighPrecisionMoney.ImplicitsString._
  import HighPrecisionMoney.ImplicitsStringPrecise._

  implicit val defaultRoundingMode = BigDecimal.RoundingMode.HALF_EVEN

  val Euro: Currency = Currency.getInstance("EUR")

  describe("High Precision Money") {

    it("should allow creation of high precision money") {
      ("0.01".EUR) must equal("0.01".EUR)
    }

    it("should not allow creation of high precision money without sufficient scale") {
      val thrown = intercept[IllegalArgumentException] {
        HighPrecisionMoney(amount = 1, fractionDigits = 2, centAmount = 1, Euro)
      }

      assert(thrown.getMessage == "requirement failed: The scale of the given amount does not match the scale of the provided currency. - 0 <-> 2")
    }

    it("should not allow creation of high precision money with less fraction digits than the currency has") {
      val thrown = intercept[IllegalArgumentException] {
        "0.01" EUR_PRECISE 1
      }

      assert(thrown.getMessage == "requirement failed: `fractionDigits` should be  >= than the default fraction digits of the currency.")
    }

    it("should convert precise amount to long value correctly") {
      ("0.0001" EUR_PRECISE 4).preciseAmountAsLong must equal (1)
    }

    it("should reduce fraction digits as expected") {
      ("0.0001" EUR_PRECISE 4).withFractionDigits(2).preciseAmountAsLong must equal(0)
    }

    it("should support the unary '-' operator.") {
      -("0.01" EUR_PRECISE 2) must equal ("-0.01" EUR_PRECISE 2)
    }

    it("should support the binary '+' operator.") {
      ("0.001" EUR_PRECISE 3) + ("0.002" EUR_PRECISE 3) must equal(
        "0.003" EUR_PRECISE 3
      )

      ("0.005" EUR_PRECISE 3) + Money.fromDecimalAmount(BigDecimal("0.01"), Euro) must equal(
        "0.015" EUR_PRECISE 3
      )

      ("0.005" EUR_PRECISE 3) + BigDecimal("0.005") must equal(
        "0.010" EUR_PRECISE 3
      )
    }

    it("should support the binary '-' operator.") {
      ("0.002" EUR_PRECISE 3) - ("0.001" EUR_PRECISE 3) must equal(
        "0.001" EUR_PRECISE 3
      )

      ("0.015" EUR_PRECISE 3) - Money.fromDecimalAmount(BigDecimal("0.01"), Euro) must equal(
        "0.005" EUR_PRECISE 3
      )

      ("0.005" EUR_PRECISE 3) - BigDecimal("0.005") must equal(
        "0.000" EUR_PRECISE 3
      )
    }

    it("should support the binary '*' operator.") {
      ("0.002" EUR_PRECISE 3) * ("5.00" EUR_PRECISE 2) must equal(
        "0.010" EUR_PRECISE 3
      )

      ("0.015" EUR_PRECISE 3) * Money.fromDecimalAmount(BigDecimal("100.00"), Euro) must equal(
        "1.500" EUR_PRECISE 3
      )

      ("0.005" EUR_PRECISE 3) * BigDecimal("0.005") must equal(
        "0.000" EUR_PRECISE 3
      )
    }

    it("should support the binary '%' operator.") {
      ("0.010" EUR_PRECISE 3) % ("5.00" EUR_PRECISE 2) must equal(
        "0.010" EUR_PRECISE 3
      )

      ("100.000" EUR_PRECISE 3) % Money.fromDecimalAmount(BigDecimal("100.00"), Euro) must equal(
        "0.000" EUR_PRECISE 3
      )

      ("0.015" EUR_PRECISE 3) % BigDecimal("0.002") must equal(
        "0.001" EUR_PRECISE 3
      )
    }

    it("should support the binary '/%' operator.") {
      ("10.000" EUR_PRECISE 3)./%(3.00) must equal(
        ("3.000" EUR_PRECISE 3, "1.000" EUR_PRECISE 3)
      )
    }

    it("should support the remainder operator.") {
      ("10.000" EUR_PRECISE 3).remainder(3.00) must equal("1.000" EUR_PRECISE 3)

      ("10.000" EUR_PRECISE 3).remainder("3.000" EUR_PRECISE 3) must equal("1.000" EUR_PRECISE 3)
    }

    it("should partition the value properly.") {
      ("10.000" EUR_PRECISE 3).partition(1, 2, 3) must equal(
        ArrayBuffer(
          "1.667" EUR_PRECISE 3,
          "3.333" EUR_PRECISE 3,
          "5.000" EUR_PRECISE 3
        )
      )
    }
  }
}