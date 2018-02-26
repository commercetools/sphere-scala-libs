import java.util.Currency

import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import org.scalatest._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class HighPrecisionMoneySpec extends FunSpec with MustMatchers {
  import HighPrecisionMoney._

  implicit val defaultRoundingMode = BigDecimal.RoundingMode.HALF_EVEN

  val Euro: Currency = Currency.getInstance("EUR")

  describe("High Precision Money") {

    it("should allow creation of high precision money") {
      (BigDecimal(0.01) EUR) must equal(BigDecimal(0.01) EUR)
    }

    it("should not allow creation of high precision money without sufficient scale") {
      val thrown = intercept[IllegalArgumentException] {
        HighPrecisionMoney(amount = 1, fractionDigits = 2, centAmount = 1, Euro)
      }

      assert(thrown.getMessage == "requirement failed: The scale of the given amount does not match the scale of the provided currency. - 0 <-> 2")
    }

    it("should not allow creation of high precision money with less fraction digits than the currency has") {
      val thrown = intercept[IllegalArgumentException] {
        BigDecimal(0.01) EUR_PRECISE 1
      }

      assert(thrown.getMessage == "requirement failed: `fractionDigits` should be  >= than the default fraction digits of the currency.")
    }

    it("should convert precise amount to long value correctly") {
      (BigDecimal("0.0001") EUR_PRECISE 4).preciseAmountAsLong must equal (1)
    }

    it("should reduce fraction digits as expected") {
      (BigDecimal("0.0001") EUR_PRECISE 4).withFractionDigits(2).preciseAmountAsLong must equal(0)
    }

    it("should support the unary '-' operator.") {
      -(BigDecimal(0.01) EUR_PRECISE 2) must equal (BigDecimal(-0.01) EUR_PRECISE 2)
    }

    it("should support the binary '+' operator.") {
      (BigDecimal(0.001) EUR_PRECISE 3) + (BigDecimal(0.002) EUR_PRECISE 3) must equal(
        BigDecimal(0.003) EUR_PRECISE 3
      )

      (BigDecimal(0.005) EUR_PRECISE 3) + Money(0.01, Euro) must equal(
        BigDecimal(0.015) EUR_PRECISE 3
      )

      (BigDecimal(0.005) EUR_PRECISE 3) + BigDecimal("0.005") must equal(
        BigDecimal("0.010") EUR_PRECISE 3
      )
    }

    it("should support the binary '-' operator.") {
      (BigDecimal(0.002) EUR_PRECISE 3) - (BigDecimal(0.001) EUR_PRECISE 3) must equal(
        BigDecimal(0.001) EUR_PRECISE 3
      )

      (BigDecimal(0.015) EUR_PRECISE 3) - Money(0.01, Euro) must equal(
        BigDecimal(0.005) EUR_PRECISE 3
      )

      (BigDecimal(0.005) EUR_PRECISE 3) - BigDecimal("0.005") must equal(
        BigDecimal(0.000) EUR_PRECISE 3
      )
    }

    it("should support the binary '*' operator.") {
      (BigDecimal(0.002) EUR_PRECISE 3) * (BigDecimal("5.00") EUR_PRECISE 2) must equal(
        BigDecimal("0.010") EUR_PRECISE 3
      )

      (BigDecimal(0.015) EUR_PRECISE 3) * Money(BigDecimal("100.00"), Euro) must equal(
        BigDecimal("1.500") EUR_PRECISE 3
      )

      (BigDecimal(0.005) EUR_PRECISE 3) * BigDecimal("0.005") must equal(
        BigDecimal("0.000") EUR_PRECISE 3
      )
    }

    it("should support the binary '%' operator.") {
      (BigDecimal("0.010") EUR_PRECISE 3) % (BigDecimal("5.00") EUR_PRECISE 2) must equal(
        BigDecimal("0.010") EUR_PRECISE 3
      )

      (BigDecimal("100.000") EUR_PRECISE 3) % Money(BigDecimal("100.00"), Euro) must equal(
        BigDecimal("0.000") EUR_PRECISE 3
      )

      (BigDecimal("0.015") EUR_PRECISE 3) % BigDecimal("0.002") must equal(
        BigDecimal("0.001") EUR_PRECISE 3
      )
    }

    it("should support the binary '/%' operator.") {
      (BigDecimal("10.000") EUR_PRECISE 3)./%(3.00) must equal(
        (BigDecimal("3.000") EUR_PRECISE 3, BigDecimal("1.000") EUR_PRECISE 3)
      )
    }

    it("should support the remainder operator.") {
      (BigDecimal("10.000") EUR_PRECISE 3).remainder(3.00) must equal(BigDecimal("1.000") EUR_PRECISE 3)

      (BigDecimal("10.000") EUR_PRECISE 3).remainder(BigDecimal("3.000") EUR_PRECISE 3) must equal(BigDecimal("1.000") EUR_PRECISE 3)
    }

    it("should partition the value properly.") {
      (BigDecimal("10.000") EUR_PRECISE 3).partition(1, 2, 3) must equal(
        ArrayBuffer(
          BigDecimal("1.667") EUR_PRECISE 3,
          BigDecimal("3.333") EUR_PRECISE 3,
          BigDecimal("5.000") EUR_PRECISE 3
        )
      )
    }
  }
}