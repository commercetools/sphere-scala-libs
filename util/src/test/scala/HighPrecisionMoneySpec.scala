import java.util.Currency

import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import org.scalatest._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class HighPrecisionMoneySpec extends FunSpec with MustMatchers {
  import Money._

  implicit val defaultRoundingMode = BigDecimal.RoundingMode.HALF_EVEN

  val Euro: Currency = Currency.getInstance("EUR")

  describe("High Precision Money") {

    it("should allow creation of high precision money") {
      HighPrecisionMoney(amount = 0.01, fractionDigits = 2, centAmount = 1, Euro) must equal(
        HighPrecisionMoney(amount = 0.01, fractionDigits = 2, centAmount = 1, Euro))
    }

    it("should not allow creation of high precision money without sufficient scale") {
      val thrown = intercept[IllegalArgumentException] {
        HighPrecisionMoney(amount = 1, fractionDigits = 2, centAmount = 1, Euro)
      }

      assert(thrown.getMessage == "requirement failed: The scale of the given amount does not match the scale of the provided currency. - 0 <-> 2")
    }

    it("should not allow creation of high precision money with less fraction digits than the currency has") {
      val thrown = intercept[IllegalArgumentException] {
        HighPrecisionMoney(amount = 0.01, fractionDigits = 1, centAmount = 1, Euro)
      }

      assert(thrown.getMessage == "requirement failed: The scale of the given amount does not match the scale of the provided currency. - 2 <-> 1")
    }

    it("should convert precise amount to long value correctly") {
      HighPrecisionMoney(amount = BigDecimal("0.0001"), fractionDigits = 4, centAmount = 0, Euro).preciseAmountAsLong must equal(1)
    }

    it("should reduce fraction digits as expected") {
      HighPrecisionMoney(amount = BigDecimal("0.0001"), fractionDigits = 4, centAmount = 0, Euro).withFractionDigits(2).preciseAmountAsLong must equal(0)
    }

    it("should support the unary '-' operator.") {
      -HighPrecisionMoney(amount = 0.01, fractionDigits = 2, centAmount = 1, Euro) must equal (HighPrecisionMoney(amount = -0.01, fractionDigits = 2, centAmount = -1, Euro))
    }

    it("should support the binary '+' operator.") {
      HighPrecisionMoney(amount = 0.001, fractionDigits = 3, centAmount = 0, Euro) +  HighPrecisionMoney(amount = 0.002, fractionDigits = 3, centAmount = 0, Euro) must equal(
        HighPrecisionMoney(amount = 0.003, fractionDigits = 3, centAmount = 0, Euro)
      )

      HighPrecisionMoney(amount = 0.005, fractionDigits = 3, centAmount = 0, Euro) +  Money(0.01, Euro) must equal(
        HighPrecisionMoney(amount = 0.015, fractionDigits = 3, centAmount = 2, Euro)
      )

      HighPrecisionMoney(amount = 0.005, fractionDigits = 3, centAmount = 0, Euro).+(BigDecimal("0.005")) must equal(
        HighPrecisionMoney(amount = BigDecimal("0.010"), fractionDigits = 3, centAmount = 1, Euro)
      )
    }

    it("should support the binary '-' operator.") {
      HighPrecisionMoney(amount = 0.002, fractionDigits = 3, centAmount = 0, Euro) -  HighPrecisionMoney(amount = 0.001, fractionDigits = 3, centAmount = 0, Euro) must equal(
        HighPrecisionMoney(amount = 0.001, fractionDigits = 3, centAmount = 0, Euro)
      )

      HighPrecisionMoney(amount = 0.015, fractionDigits = 3, centAmount = 0, Euro) -  Money(0.01, Euro) must equal(
        HighPrecisionMoney(amount = 0.005, fractionDigits = 3, centAmount = 0, Euro)
      )

      HighPrecisionMoney(amount = 0.005, fractionDigits = 3, centAmount = 0, Euro).-(BigDecimal("0.005")) must equal(
        HighPrecisionMoney(amount = BigDecimal("0.000"), fractionDigits = 3, centAmount = 0, Euro)
      )
    }

    it("should support the binary '*' operator.") {
      HighPrecisionMoney(amount = 0.002, fractionDigits = 3, centAmount = 0, Euro) *  HighPrecisionMoney(amount = BigDecimal("5.00"), fractionDigits = 2, centAmount = 500, Euro) must equal(
        HighPrecisionMoney(amount = BigDecimal("0.010"), fractionDigits = 3, centAmount = 1, Euro)
      )

      HighPrecisionMoney(amount = 0.015, fractionDigits = 3, centAmount = 0, Euro) *  Money(BigDecimal("100.00"), Euro) must equal(
        HighPrecisionMoney(amount = BigDecimal("1.500"), fractionDigits = 3, centAmount = 150, Euro)
      )

      HighPrecisionMoney(amount = 0.005, fractionDigits = 3, centAmount = 0, Euro).*(BigDecimal("0.005")) must equal(
        HighPrecisionMoney(amount = BigDecimal("0.000"), fractionDigits = 3, centAmount = 0, Euro)
      )
    }

    it("should support the binary '%' operator.") {
      HighPrecisionMoney(amount = 0.002, fractionDigits = 3, centAmount = 0, Euro) *  HighPrecisionMoney(amount = BigDecimal("5.00"), fractionDigits = 2, centAmount = 500, Euro) must equal(
        HighPrecisionMoney(amount = BigDecimal("0.010"), fractionDigits = 3, centAmount = 1, Euro)
      )

      HighPrecisionMoney(amount = 0.015, fractionDigits = 3, centAmount = 0, Euro) *  Money(BigDecimal("100.00"), Euro) must equal(
        HighPrecisionMoney(amount = BigDecimal("1.500"), fractionDigits = 3, centAmount = 150, Euro)
      )

      HighPrecisionMoney(amount = 0.005, fractionDigits = 3, centAmount = 0, Euro).*(BigDecimal("0.005")) must equal(
        HighPrecisionMoney(amount = BigDecimal("0.000"), fractionDigits = 3, centAmount = 0, Euro)
      )
    }

    it("should support the binary '/%' operator.") {
      HighPrecisionMoney(amount = BigDecimal("10.000"), fractionDigits = 3, centAmount = 1000, Euro)./%(3.00) must equal(
        (HighPrecisionMoney(amount = BigDecimal("3.000"), fractionDigits = 3, centAmount = 300, Euro),
          HighPrecisionMoney(amount = BigDecimal("1.000"), fractionDigits = 3, centAmount = 100, Euro))
      )
    }

    it("should support the remainder operator.") {
      HighPrecisionMoney(amount = BigDecimal("10.000"), fractionDigits = 3, centAmount = 1000, Euro).remainder(3.00) must equal(
        HighPrecisionMoney(amount = BigDecimal("1.000"), fractionDigits = 3, centAmount = 100, Euro))

      HighPrecisionMoney(amount = BigDecimal("10.000"), fractionDigits = 3, centAmount = 1000, Euro).remainder(
        HighPrecisionMoney(amount = BigDecimal("3.000"), fractionDigits = 3, centAmount = 300, Euro)) must equal(
          HighPrecisionMoney(amount = BigDecimal("1.000"), fractionDigits = 3, centAmount = 100, Euro))
    }

    it("should partition the value properly.") {
      val a = HighPrecisionMoney(amount = BigDecimal("10.000"), fractionDigits = 3, centAmount = 1000, Euro).partition(1, 2, 3) must equal(
        ArrayBuffer(
          HighPrecisionMoney(amount = BigDecimal("1.667"), fractionDigits = 3, centAmount = 167, Euro),
          HighPrecisionMoney(amount = BigDecimal("3.333"), fractionDigits = 3, centAmount = 333, Euro),
          HighPrecisionMoney(amount = BigDecimal("5.000"), fractionDigits = 3, centAmount = 500, Euro)
        )
      )
    }

    //make constructors private, only use from centAmount, fromDecimalAmount

  }
}