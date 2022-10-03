package io.sphere.util

import cats.data.Validated.Invalid
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.Table
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.util.Currency
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.math.BigDecimal.RoundingMode

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

    it("should support the binary '/%' operator.") {
      "10.000".EUR_PRECISE(3)./%(3.00) must equal(
        ("3.000".EUR_PRECISE(3), "1.000".EUR_PRECISE(3))
      )
    }

    it("should support the remainder operator.") {
      "10.000".EUR_PRECISE(3).remainder(3.00) must equal("1.000".EUR_PRECISE(3))

      "10.000".EUR_PRECISE(3).remainder("3.000".EUR_PRECISE(3)) must equal("1.000".EUR_PRECISE(3))
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

    it("should provide convenient toString") {
      "10.000".EUR_PRECISE(3).toString must be("10.000 EUR")
    }

    it("should not fail on toString") {
      forAll(DomainObjectsGen.highPrecisionMoney) { m =>
        m.toString
      }
    }

    it("roundFloor should behave similarly to BigDecimal rounding") {
      ScalaCheckDrivenPropertyChecks.forAll(DomainObjectsGen.highPrecisionMoney) { h =>
        val bdRes = HighPrecisionMoney.roundToCents(h.amount, h.currency)(RoundingMode.FLOOR)
        val longRes =
          HighPrecisionMoney.roundFloor(
            h.preciseAmount,
            h.fractionDigits,
            h.currency.getDefaultFractionDigits)
        bdRes must be(longRes)
      }
    }

    it("roundCeiling should behave similarly to BigDecimal rounding") {
      ScalaCheckDrivenPropertyChecks.forAll(DomainObjectsGen.highPrecisionMoney) { h =>
        val bdRes = HighPrecisionMoney.roundToCents(h.amount, h.currency)(RoundingMode.CEILING)
        val longRes =
          HighPrecisionMoney.roundCeiling(
            h.preciseAmount,
            h.fractionDigits,
            h.currency.getDefaultFractionDigits)
        bdRes must be(longRes)
      }
    }

    it("roundHalfEven should behave similarly to BigDecimal rounding") {
      // I used random generated values later, but I needed these very specific values too to check the
      // edge cases of the half even rounding
      val data = Table(
        ("preciseAmount", "fraction", "currency"),
        (1119, 3, Euro),
        (1111, 3, Euro),
        (1115, 3, Euro),
        (1125, 3, Euro),
        (112500, 5, Euro),
        (11250001, 7, Euro),
        (11000004, 7, Euro),
        (11249999, 7, Euro),
        (-1119, 3, Euro),
        (-1111, 3, Euro),
        (-1115, 3, Euro),
        (-1125, 3, Euro),
        (-112500, 5, Euro)
      )

      TableDrivenPropertyChecks.forAll(data) { (preciseAmount, fd, cur) =>
        val h = HighPrecisionMoney.fromPreciseAmount(preciseAmount, fd, cur, None).getOrElse(null)

        val bdRes = HighPrecisionMoney.roundToCents(h.amount, cur)(RoundingMode.HALF_EVEN)

        val longRes =
          HighPrecisionMoney.roundHalfEven(preciseAmount, fd, cur.getDefaultFractionDigits)

        bdRes must be(longRes)
      }

      ScalaCheckDrivenPropertyChecks.forAll(DomainObjectsGen.highPrecisionMoney) { h =>
        val bdRes = HighPrecisionMoney.roundToCents(h.amount, h.currency)(RoundingMode.HALF_EVEN)

        val longRes =
          HighPrecisionMoney.roundHalfEven(
            h.preciseAmount,
            h.fractionDigits,
            h.currency.getDefaultFractionDigits)

        bdRes must be(longRes)
      }

    }

  }
}
