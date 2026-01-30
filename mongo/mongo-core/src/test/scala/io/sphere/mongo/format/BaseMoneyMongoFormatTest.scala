package io.sphere.mongo.format

import java.util.Currency

import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import DefaultMongoFormats._
import io.sphere.mongo.MongoUtils._
import org.bson.BSONObject
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._

class BaseMoneyMongoFormatTest extends AnyWordSpec with Matchers {

  "MongoFormat[BaseMoney]" should {
    "be symmetric" in {
      val money = Money.EUR(34.56)
      val f = MongoFormat[Money]
      val dbo = f.toMongoValue(money)
      val readMoney = f.fromMongoValue(dbo)

      money should be(readMoney)
    }

    "decode with type info" in {
      val dbo = dbObj(
        "type" -> "centPrecision",
        "currencyCode" -> "USD",
        "centAmount" -> 3298
      )

      MongoFormat[BaseMoney].fromMongoValue(dbo) should be(Money.USD(BigDecimal("32.98")))
    }

    "decode without type info" in {
      val dbo = dbObj(
        "currencyCode" -> "USD",
        "centAmount" -> 3298
      )

      MongoFormat[BaseMoney].fromMongoValue(dbo) should be(Money.USD(BigDecimal("32.98")))
    }
  }

  "MongoFormat[HighPrecisionMoney]" should {
    "be symmetric" in {
      implicit val mode = BigDecimal.RoundingMode.HALF_EVEN

      val money = HighPrecisionMoney.fromDecimalAmount(34.123456, 6, Currency.getInstance("EUR"))
      val dbo = MongoFormat[HighPrecisionMoney].toMongoValue(money)

      val decodedMoney = MongoFormat[HighPrecisionMoney].fromMongoValue(dbo)
      val decodedBaseMoney = MongoFormat[BaseMoney].fromMongoValue(dbo)

      decodedMoney should equal(money)
      decodedBaseMoney should equal(money)
    }

    "decode with type info" in {
      val dbo = dbObj(
        "type" -> "highPrecision",
        "currencyCode" -> "USD",
        "preciseAmount" -> 42,
        "fractionDigits" -> 4
      )

      MongoFormat[BaseMoney].fromMongoValue(dbo) should be(
        HighPrecisionMoney.USD(BigDecimal("0.0042"), Some(4)))
    }

    "decode with centAmount" in {
      val dbo = dbObj(
        "type" -> "highPrecision",
        "currencyCode" -> "USD",
        "preciseAmount" -> 42,
        "centAmount" -> 1,
        "fractionDigits" -> 4
      )

      val parsed = MongoFormat[BaseMoney].fromMongoValue(dbo)
      MongoFormat[BaseMoney].toMongoValue(parsed).asInstanceOf[BSONObject].toMap.asScala should be(
        dbo.toMap.asScala)
    }

    "validate data when decoded from JSON" in {
      val dbo = dbObj(
        "type" -> "highPrecision",
        "currencyCode" -> "USD",
        "preciseAmount" -> 42,
        "fractionDigits" -> 1
      )

      an[Exception] shouldBe thrownBy(MongoFormat[BaseMoney].fromMongoValue(dbo))
    }
  }

}
