package io.sphere.json

import cats.data.Validated.{Invalid, Valid}
import io.sphere.util.CustomCurrency.HUF0
import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import io.sphere.util.test._
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
      val readAst = parseJSON(jsonAsString).expectValid

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
      val readAst = parseJSON(jsonAsString).expectValid

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
      val readAst = parseJSON(jsonAsString).expectValid
      val decodedMoney = fromJSON[HighPrecisionMoney](jsonAsString).expectValid
      val decodedBaseMoney = fromJSON[BaseMoney](jsonAsString).expectValid

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
      val json = parseJSON("""
        {
          "type": "highPrecision",
          "currencyCode": "USD",
          "preciseAmount": 42,
          "centAmount": 1,
          "fractionDigits": 4
        }
        """).expectValid

      val parsed = fromJValue[BaseMoney](json).expectValid

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

    "fail to decode high precision money with fractionDigits equal to currency default" in {
      val json =
        """
        {
         "type": "highPrecision",
         "currencyCode": "HUF0",
         "centAmount": 100,
         "preciseAmount": 100500,
         "fractionDigits": 0
        }
        """

      fromJSON[BaseMoney](json) match {
        case Invalid(errors) =>
          errors.toList.map(_.toString) should be(
            List("fractionDigits must be > 0 (default fraction digits defined by currency HUF0)."))
        case Valid(_) => fail("Expected Invalid but got Valid")
      }
    }
  }

}
