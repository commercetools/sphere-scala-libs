package io.sphere.json

import java.util.Currency

import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import cats.data.Validated.Valid
import org.json4s.jackson.compactJson
import org.scalatest.{Matchers, WordSpec}

class MoneySymmetrySpec extends WordSpec with Matchers {
  "money encoding/decoding" should {
    "be symetric" in {
      val money = Money.EUR(34.56)
      val jsonAst = toJValue(money)
      val jsonAsString = compactJson(jsonAst)
      val Valid(readAst) = parseJSON(jsonAsString)

      jsonAst should equal (readAst)
    }
  }

  "High precision money encoding/decoding" should {
    "be symetric" in {
      implicit val mode = BigDecimal.RoundingMode.HALF_EVEN

      val money = HighPrecisionMoney.makeWithoutCents(34.123456, 6, Currency.getInstance("EUR"))
      val jsonAst = toJValue(money)
      val jsonAsString = compactJson(jsonAst)
      val Valid(readAst) = parseJSON(jsonAsString)
      val Valid(decodedMoney) = fromJSON[HighPrecisionMoney](jsonAsString)
      val Valid(decodedBaseMoney) = fromJSON[BaseMoney](jsonAsString)

      jsonAst should equal (readAst)
      decodedMoney should equal (money)
      decodedBaseMoney should equal (money)
    }
  }

}
