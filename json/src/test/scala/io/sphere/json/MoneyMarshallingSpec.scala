package io.sphere.json

import java.util.Currency

import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import cats.data.Validated.Valid
import org.json4s.jackson.compactJson
import org.scalatest.{Matchers, WordSpec}

class MoneyMarshallingSpec extends WordSpec with Matchers {
  "money encoding/decoding" should {
    "be symmetric" in {
      val money = Money.EUR(34.56)
      val jsonAst = toJValue(money)
      val jsonAsString = compactJson(jsonAst)
      val Valid(readAst) = parseJSON(jsonAsString)

      jsonAst should equal (readAst)
    }

    "decode with type info" in {
      val json =
        """
        {
          "type" : "centPrecision",
          "currencyCode" : "USD",
          "centAmount" : 3298
        }
        """

      fromJSON[BaseMoney](json) should be (Valid(Money.USD(BigDecimal("32.98"))))
    }

    "decode without type info" in {
      val json =
        """
        {
          "currencyCode" : "USD",
          "centAmount" : 3298
        }
        """

      fromJSON[BaseMoney](json) should be (Valid(Money.USD(BigDecimal("32.98"))))
    }
  }

  "High precision money encoding/decoding" should {
    "be symmetric" in {
      implicit val mode = BigDecimal.RoundingMode.HALF_EVEN

      val money = HighPrecisionMoney.fromDecimalAmount(34.123456, 6, Currency.getInstance("EUR"))
      val jsonAst = toJValue(money)
      val jsonAsString = compactJson(jsonAst)
      val Valid(readAst) = parseJSON(jsonAsString)
      val Valid(decodedMoney) = fromJSON[HighPrecisionMoney](jsonAsString)
      val Valid(decodedBaseMoney) = fromJSON[BaseMoney](jsonAsString)

      jsonAst should equal (readAst)
      decodedMoney should equal (money)
      decodedBaseMoney should equal (money)
    }

    "decode with type info" in {
      val json =
        """
        {
          "type": "highPrecision",
          "currencyCode": "USD",
          "preciseAmount": 42,
          "fractionDigits": 4
        }
        """

      fromJSON[BaseMoney](json) should be (Valid(HighPrecisionMoney.USD(BigDecimal("0.0042"), Some(4))))
    }

    "validate data when decoded from JSON" in {
      val json =
        """
        {
          "type": "highPrecision",
          "currencyCode": "USD",
          "preciseAmount": 42,
          "fractionDigits": 1
        }
        """

      fromJSON[BaseMoney](json).isValid should be (false)
    }
  }

}
