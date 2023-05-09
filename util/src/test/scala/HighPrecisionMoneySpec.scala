package io.sphere.util

import java.util.Currency
import cats.data.Validated.Invalid
import io.sphere.util.HighPrecisionMoney.ImplicitsDecimalPrecise.HighPrecisionPreciseMoneyNotation
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class HighPrecisionMoneySpec extends AnyFunSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  import HighPrecisionMoney.ImplicitsString._
  import HighPrecisionMoney.ImplicitsStringPrecise._

  implicit val defaultRoundingMode = BigDecimal.RoundingMode.HALF_EVEN

  val Euro: Currency = Currency.getInstance("EUR")

  describe("High Precision Money") {

    it("should allow creation of high precision money") {
      ("0.01".EUR) must equal("0.01".EUR)
    }

    it(
      "should not allow creation of high precision money with less fraction digits than the currency has") {
      val thrown = intercept[IllegalArgumentException] {
        "0.01".EUR_PRECISE(1)
      }

      assert(
        thrown.getMessage == "requirement failed: `fractionDigits` should be  >= than the default fraction digits of the currency.")
    }

    it("should convert precise amount to long value correctly") {
      "0.0001".EUR_PRECISE(4).preciseAmount must equal(1)
    }

    it("should reduce fraction digits as expected") {
      "0.0001".EUR_PRECISE(4).withFractionDigits(2).preciseAmount must equal(0)
    }

    it("should support the unary '-' operator.") {
      -"0.01".EUR_PRECISE(2) must equal("-0.01".EUR_PRECISE(2))
    }

    it("should throw error on overflow in the unary '-' operator.") {
      a[MoneyOverflowException] must be thrownBy {
        -(BigDecimal(Long.MinValue) / 1000).EUR_PRECISE(3)
      }
    }

    it("should support the binary '+' operator.") {
      ("0.001".EUR_PRECISE(3)) + ("0.002".EUR_PRECISE(3)) must equal(
        "0.003".EUR_PRECISE(3)
      )

      ("0.005".EUR_PRECISE(3)) + Money.fromDecimalAmount(BigDecimal("0.01"), Euro) must equal(
        "0.015".EUR_PRECISE(3)
      )

      ("0.005".EUR_PRECISE(3)) + BigDecimal("0.005") must equal(
        "0.010".EUR_PRECISE(3)
      )
    }

    it("should throw error on overflow in the binary '+' operator.") {
      a[MoneyOverflowException] must be thrownBy {
        (BigDecimal(Long.MaxValue) / 1000).EUR_PRECISE(3) + 1
      }
    }

    it("should support the binary '-' operator.") {
      ("0.002".EUR_PRECISE(3)) - ("0.001".EUR_PRECISE(3)) must equal(
        "0.001".EUR_PRECISE(3)
      )

      ("0.015".EUR_PRECISE(3)) - Money.fromDecimalAmount(BigDecimal("0.01"), Euro) must equal(
        "0.005".EUR_PRECISE(3)
      )

      ("0.005".EUR_PRECISE(3)) - BigDecimal("0.005") must equal(
        "0.000".EUR_PRECISE(3)
      )
    }

    it("should throw error on overflow in the binary '-' operator.") {
      a[MoneyOverflowException] must be thrownBy {
        (BigDecimal(Long.MinValue) / 1000).EUR_PRECISE(3) - 1
      }
    }

    it("should support the binary '*' operator.") {
      ("0.002".EUR_PRECISE(3)) * ("5.00".EUR_PRECISE(2)) must equal(
        "0.010".EUR_PRECISE(3)
      )

      ("0.015".EUR_PRECISE(3)) * Money.fromDecimalAmount(BigDecimal("100.00"), Euro) must equal(
        "1.500".EUR_PRECISE(3)
      )

      ("0.005".EUR_PRECISE(3)) * BigDecimal("0.005") must equal(
        "0.000".EUR_PRECISE(3)
      )
    }

    it("should throw error on overflow in the binary '*' operator.") {
      a[MoneyOverflowException] must be thrownBy {
        (BigDecimal(Long.MaxValue / 1000) / 2 + 1).EUR_PRECISE(3) * 2
      }
    }

    it("should support the binary '%' operator.") {
      ("0.010".EUR_PRECISE(3)) % ("5.00".EUR_PRECISE(2)) must equal(
        "0.010".EUR_PRECISE(3)
      )

      ("100.000".EUR_PRECISE(3)) % Money.fromDecimalAmount(BigDecimal("100.00"), Euro) must equal(
        "0.000".EUR_PRECISE(3)
      )

      ("0.015".EUR_PRECISE(3)) % BigDecimal("0.002") must equal(
        "0.001".EUR_PRECISE(3)
      )
    }

    it("should throw error on overflow in the binary '%' operator.") {
      noException must be thrownBy {
        BigDecimal(Long.MaxValue / 1000).EUR_PRECISE(3) % 0.5
      }
    }

    it("should support the binary '/%' operator.") {
      "10.000".EUR_PRECISE(3)./%(3.00) must equal(
        ("3.000".EUR_PRECISE(3), "1.000".EUR_PRECISE(3))
      )
    }

    it("should throw error on overflow in the binary '/%' operator.") {
      a[MoneyOverflowException] must be thrownBy {
        BigDecimal(Long.MaxValue / 1000).EUR_PRECISE(3) /% 0.5
      }
    }

    it("should support the remainder operator.") {
      "10.000".EUR_PRECISE(3).remainder(3.00) must equal("1.000".EUR_PRECISE(3))

      "10.000".EUR_PRECISE(3).remainder("3.000".EUR_PRECISE(3)) must equal("1.000".EUR_PRECISE(3))
    }

    it("should not overflow when getting the remainder of a division ('%').") {
      noException must be thrownBy {
        BigDecimal(Long.MaxValue / 1000).EUR_PRECISE(3).remainder(0.5)
      }
    }

    it("should partition the value properly.") {
      "10.000".EUR_PRECISE(3).partition(1, 2, 3) must equal(
        ArrayBuffer(
          "1.667".EUR_PRECISE(3),
          "3.333".EUR_PRECISE(3),
          "5.000".EUR_PRECISE(3)
        )
      )
    }

    it("should validate fractionDigits (min)") {
      val Invalid(errors) = HighPrecisionMoney.fromPreciseAmount(123456L, 1, Euro, None)

      errors.toList must be(
        List("fractionDigits must be > 2 (default fraction digits defined by currency EUR)."))
    }

    it("should validate fractionDigits (max)") {
      val Invalid(errors) = HighPrecisionMoney.fromPreciseAmount(123456L, 100, Euro, None)

      errors.toList must be(List("fractionDigits must be <= 20."))
    }

    it("should validate centAmount") {
      val Invalid(errors) = HighPrecisionMoney.fromPreciseAmount(123456L, 4, Euro, Some(1))

      errors.toList must be(
        List(
          "centAmount must be correctly rounded preciseAmount (a number between 1234 and 1235)."))
    }

    it("should provide convenient toString") {
      "10.000".EUR_PRECISE(3).toString must be("10.000 EUR")
      "0.100".EUR_PRECISE(3).toString must be("0.100 EUR")
      "0.010".EUR_PRECISE(3).toString must be("0.010 EUR")
      "0.000".EUR_PRECISE(3).toString must be("0.000 EUR")
      "94.500".EUR_PRECISE(3).toString must be("94.500 EUR")
      "94".JPY_PRECISE(0).toString must be("94 JPY")
    }

    it("should not fail on toString") {
      forAll(DomainObjectsGen.highPrecisionMoney) { m =>
        m.toString
      }
    }

    it("should fail on too big fraction decimal") {
      val thrown = intercept[IllegalArgumentException] {
        val tooManyDigits = Euro.getDefaultFractionDigits + 19
        HighPrecisionMoney.fromCentAmount(100003, tooManyDigits, Euro)
      }

      assert(thrown.getMessage == "Cannot represent number bigger than 10^19 with a Long")
    }
  }
}
