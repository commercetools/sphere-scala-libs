package io.sphere.json

import cats.data.Validated.Valid
import io.sphere.util.CustomCurrency.HUF0
import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import org.json4s.jackson.compactJson
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.Currency

class MoneyMarshallingSpec extends AnyWordSpec with Matchers {
  "money encoding/decoding" should {
    "be symmetric" in {
      val money = Money.EUR(34.56)
      val jsonAst = toJValue(money)
      val jsonAsString = compactJson(jsonAst)
      val Valid(readAst) = parseJSON(jsonAsString)

      jsonAst should equal(readAst)
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

      fromJSON[BaseMoney](json) should be(Valid(Money.USD(BigDecimal("32.98"))))
    }

    "decode without type info" in {
      val json =
        """
        {
          "currencyCode" : "USD",
          "centAmount" : 3298
        }
        """

      fromJSON[BaseMoney](json) should be(Valid(Money.USD(BigDecimal("32.98"))))
    }

    "be symmetric for custom currencies" in {
      val money = Money.fromCentAmount(1234, HUF0)
      val jsonAst = toJValue(money)
      val jsonAsString = compactJson(jsonAst)
      val Valid(readAst) = parseJSON(jsonAsString)

      val json =
        """
        {
          "currencyCode" : "HUF0",
          "centAmount" : 1234
        }
        """
      fromJSON[BaseMoney](json) should be(Valid(money))

      jsonAst should equal(readAst)
    }

    "serialize custom currencies with their unique code" in {
      val money = Money.fromCentAmount(1234, HUF0)

      val expectedJson =
        """{"type":"centPrecision","currencyCode":"HUF0","centAmount":1234,"fractionDigits":0}"""
      compactJson(toJValue(money)) should be(expectedJson)
    }

    "serialize the ISO currency counterpart of a custom currency" in {
      val money = Money.fromCentAmount(1234, Currency.getInstance("HUF"))

      val expectedJson =
        """{"type":"centPrecision","currencyCode":"HUF","centAmount":1234,"fractionDigits":2}"""
      compactJson(toJValue(money)) should be(expectedJson)
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

      jsonAst should equal(readAst)
      decodedMoney should equal(money)
      decodedBaseMoney should equal(money)
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

      fromJSON[BaseMoney](json) should be(
        Valid(HighPrecisionMoney.USD(BigDecimal("0.0042"), Some(4))))
    }

    "decode with centAmount" in {
      val Valid(json) = parseJSON("""
        {
          "type": "highPrecision",
          "currencyCode": "USD",
          "preciseAmount": 42,
          "centAmount": 1,
          "fractionDigits": 4
        }
        """)

      val Valid(parsed) = fromJValue[BaseMoney](json)

      toJValue(parsed) should be(json)
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

      fromJSON[BaseMoney](json).isValid should be(false)
    }
  }

}
