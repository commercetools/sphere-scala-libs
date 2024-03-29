package io.sphere.util

import java.util.Currency

import org.scalacheck.Gen

import scala.collection.JavaConverters._

object DomainObjectsGen {

  private val currency: Gen[Currency] =
    Gen.oneOf(Currency.getAvailableCurrencies.asScala.toSeq)

  val money: Gen[Money] = for {
    currency <- currency
    amount <- Gen.chooseNum[Long](Long.MinValue, Long.MaxValue)
  } yield Money(amount, currency)

  val highPrecisionMoney: Gen[HighPrecisionMoney] = for {
    money <- money
  } yield HighPrecisionMoney.fromMoney(money, money.currency.getDefaultFractionDigits)

  val baseMoney: Gen[BaseMoney] = Gen.oneOf(money, highPrecisionMoney)

}
