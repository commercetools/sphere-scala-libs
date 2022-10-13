package io.sphere.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.Table
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.util.Currency
import scala.math.BigDecimal.RoundingMode

class MoneyRoundingSpec extends AnyFunSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  val Euro: Currency = Currency.getInstance("EUR")
  val ZWL: Currency = Currency.getInstance("ZWL")
  val JPY: Currency = Currency.getInstance("JPY")

  describe("Money Rounding") {
    it("roundFloor should behave similarly to BigDecimal rounding") {
      ScalaCheckDrivenPropertyChecks.forAll(DomainObjectsGen.highPrecisionMoney) { h =>
        val bdRes = HighPrecisionMoney.roundToCents(h.amount, h.currency)(RoundingMode.FLOOR)
        val longRes =
          MoneyRounding.roundFloor(h.preciseAmount, h.fractionDigits, h.currency)
        bdRes must be(longRes)
      }
    }

    it("roundCeiling should behave similarly to BigDecimal rounding") {
      ScalaCheckDrivenPropertyChecks.forAll(DomainObjectsGen.highPrecisionMoney) { h =>
        val bdRes = HighPrecisionMoney.roundToCents(h.amount, h.currency)(RoundingMode.CEILING)
        val longRes =
          MoneyRounding.roundCeiling(h.preciseAmount, h.fractionDigits, h.currency)
        bdRes must be(longRes)
      }
    }

    it("roundHalfEven should behave similarly to BigDecimal rounding") {
      // I used random generated values later, but I needed these very specific values too to check the
      // edge cases of the half even rounding
      val data = Table(
        ("preciseAmount", "fraction", "currency"),
        (1119L, 3, Euro),
        (1111L, 3, Euro),
        (1115L, 3, Euro),
        (1125L, 3, Euro),
        (112500L, 5, Euro),
        (11250001L, 7, Euro),
        (11000004L, 7, Euro),
        (11249999L, 7, Euro),
        (-1119L, 3, Euro),
        (-1111L, 3, Euro),
        (-1115L, 3, Euro),
        (-1125L, 3, Euro),
        (-112500L, 5, Euro),
        (5721482481806080960L, 6, ZWL),
        (123L, 0, JPY)
      )

      TableDrivenPropertyChecks.forAll(data) { (preciseAmount, fd, cur) =>
        val amount = HighPrecisionMoney.preciseAmountToAmount(preciseAmount, fd)
        val bdRes = HighPrecisionMoney.roundToCents(amount, cur)(RoundingMode.HALF_EVEN)

        val longRes =
          MoneyRounding.roundHalfEven(preciseAmount, fd, cur)

        bdRes must be(longRes)
      }

      ScalaCheckDrivenPropertyChecks.forAll(DomainObjectsGen.highPrecisionMoney) { h =>
        val bdRes = HighPrecisionMoney.roundToCents(h.amount, h.currency)(RoundingMode.HALF_EVEN)

        val longRes =
          MoneyRounding.roundHalfEven(h.preciseAmount, h.fractionDigits, h.currency)

        bdRes must be(longRes)
      }
    }
  }
}
