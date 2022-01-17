package io.sphere.util

import java.util.Currency

import cats.data.Validated.Invalid

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class HighPrecisionMoneyOperationSpec
    extends AnyFunSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {
  import HighPrecisionMoneyOperation.ImplicitsString._
  import HighPrecisionMoneyOperation.ImplicitsStringPrecise._

  implicit val defaultRoundingMode = BigDecimal.RoundingMode.HALF_EVEN

  val Euro: Currency = Currency.getInstance("EUR")

  describe("High Precision Money") {

    it("should allow creation of high precision money") {
      ("0.01".EUR) must equal("0.01".EUR)
    }

    it("should not allow creation of high precision money without sufficient scale") {
      val thrown = intercept[IllegalArgumentException] {
        HighPrecisionMoneyOperation(amount = 1, fractionDigits = 2, Euro)
      }

      assert(
        thrown.getMessage == "requirement failed: The scale of the given amount does not match the scale of the provided currency. - 0 <-> 2")
    }

    it(
      "should not allow creation of high precision money with less fraction digits than the currency has") {
      val thrown = intercept[IllegalArgumentException] {
        "0.01".EUR_PRECISE(1)
      }

      assert(
        thrown.getMessage == "requirement failed: `fractionDigits` should be  >= than the default fraction digits of the currency.")
    }

    it("should support the unary '-' operator.") {
      -"0.01".EUR_PRECISE(2) must equal("-0.01".EUR_PRECISE(2))
    }

    it("should support the binary '+' operator.") {
      ("0.001".EUR_PRECISE(3)) + ("0.002".EUR_PRECISE(3)) must equal(
        "0.003".EUR_PRECISE(3)
      )

      ("0.005".EUR_PRECISE(3)) + MoneyOperation.fromDecimalAmount(
        BigDecimal("0.01"),
        Euro) must equal(
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

      ("0.015".EUR_PRECISE(3)) - MoneyOperation.fromDecimalAmount(
        BigDecimal("0.01"),
        Euro) must equal(
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

      ("0.015".EUR_PRECISE(3)) * MoneyOperation.fromDecimalAmount(
        BigDecimal("100.00"),
        Euro) must equal(
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

      ("100.000".EUR_PRECISE(3)) % MoneyOperation.fromDecimalAmount(
        BigDecimal("100.00"),
        Euro) must equal(
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
      val Invalid(errors) = HighPrecisionMoneyOperation.fromPreciseAmount(123456L, 1, Euro)

      errors.toList must be(
        List("fractionDigits must be > 2 (default fraction digits defined by currency EUR)."))
    }

    it("should validate fractionDigits (max)") {
      val Invalid(errors) = HighPrecisionMoneyOperation.fromPreciseAmount(123456L, 100, Euro)

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
  }
}
