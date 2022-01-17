package io.sphere.util

import language.implicitConversions
import java.math.MathContext
import java.text.NumberFormat
import java.util.{Currency, Locale}

import cats.Monoid
import cats.data.ValidatedNel
import cats.syntax.validated._

import scala.math._
import BigDecimal.RoundingMode._
import scala.math.BigDecimal.RoundingMode
import ValidatedFlatMapFeature._
import io.sphere.util.Money.ImplicitsDecimal.MoneyNotation

sealed trait BaseMoney {
  def `type`: String

  def currency: Currency

  // Use with CAUTION! will loose precision in case of a high precision money value
  def centAmount: Long

  def fractionDigits: Int

  def toMoneyWithPrecisionLoss: Money
  def operation: BaseMoneyOperation
}

object BaseMoney {
  val TypeField: String = "type"

  def requireSameCurrency(m1: BaseMoney, m2: BaseMoney): Unit =
    require(m1.currency eq m2.currency, s"${m1.currency} != ${m2.currency}")

  def toScalaRoundingMode(mode: java.math.RoundingMode): RoundingMode =
    BigDecimal.RoundingMode(mode.ordinal())
}

/** Represents an amount of money in a certain currency.
  *
  * This implementation does not support fractional money units (eg a tenth cent). Amounts are
  * always rounded to the nearest, smallest unit of the respective currency. The rounding mode can
  * be specified using an implicit `BigDecimal.RoundingMode`.
  *
  * @param amount
  *   The amount as a decimal value. The scale must be equal to or less than the number of
  *   fractional digits of the currency.
  * @param currency
  *   The currency of the amount.
  */
case class Money private (amount: BigDecimal, currency: Currency)
    extends BaseMoney
    with Ordered[Money] {
  import Money._

  require(
    amount.scale == currency.getDefaultFractionDigits,
    "The scale of the given amount does not match the scale of the provided currency." +
      " - " + amount.scale + " <-> " + currency.getDefaultFractionDigits
  )

  private val centFactor: Double = 1 / pow(10, currency.getDefaultFractionDigits)

  lazy val centAmount: Long = (amount / centFactor).toLong

  private val backwardsCompatibleRoundingModeForOperations = BigDecimal.RoundingMode.HALF_EVEN

  val `type`: String = TypeName

  lazy val fractionDigits: Int = currency.getDefaultFractionDigits

  override def operation: MoneyOperation = MoneyOperation.fromCentAmount(centAmount, currency)

  def withCentAmount(centAmount: Long): Money = {
    val newAmount = BigDecimal(centAmount) * centFactor
    copy(amount = newAmount.setScale(currency.getDefaultFractionDigits))
  }

  def toHighPrecisionMoney(fractionDigits: Int): HighPrecisionMoney =
    HighPrecisionMoney.fromMoney(this, fractionDigits)

  /** Creates a new Money instance with the same currency and the amount conforming to the given
    * MathContext (scale and rounding mode).
    */
  def apply(mc: MathContext): Money =
    fromDecimalAmount(this.amount(mc), this.currency)(RoundingMode.HALF_EVEN)

  def toMoneyWithPrecisionLoss: Money = this

  def compare(that: Money): Int = {
    BaseMoney.requireSameCurrency(this, that)
    this.amount.compare(that.amount)
  }

  override def toString: String =
    this.amount.bigDecimal.toPlainString + " " + this.currency.getCurrencyCode

  def toString(nf: NumberFormat, locale: Locale): String = {
    require(nf.getCurrency eq this.currency)
    nf.format(this.amount.doubleValue) + " " + this.currency.getSymbol(locale)
  }
}

object Money {
  object ImplicitsDecimal {
    final implicit class MoneyNotation(val amount: BigDecimal) extends AnyVal {
      def EUR: Money = Money.EUR(amount)
      def USD: Money = Money.USD(amount)
      def GBP: Money = Money.GBP(amount)
      def JPY: Money = Money.JPY(amount)
    }

    implicit def doubleMoneyNotation(amount: Double): MoneyNotation =
      new ImplicitsDecimal.MoneyNotation(BigDecimal(amount))
  }

  object ImplicitsString {
    implicit def stringMoneyNotation(amount: String): MoneyNotation =
      new ImplicitsDecimal.MoneyNotation(BigDecimal(amount))
  }

  private def decimalAmountWithCurrencyAndHalfEvenRounding(amount: BigDecimal, currency: String) =
    fromDecimalAmount(amount, Currency.getInstance(currency))(BigDecimal.RoundingMode.HALF_EVEN)

  def EUR(amount: BigDecimal): Money = decimalAmountWithCurrencyAndHalfEvenRounding(amount, "EUR")
  def USD(amount: BigDecimal): Money = decimalAmountWithCurrencyAndHalfEvenRounding(amount, "USD")
  def GBP(amount: BigDecimal): Money = decimalAmountWithCurrencyAndHalfEvenRounding(amount, "GBP")
  def JPY(amount: BigDecimal): Money = decimalAmountWithCurrencyAndHalfEvenRounding(amount, "JPY")

  val CurrencyCodeField: String = "currencyCode"
  val CentAmountField: String = "centAmount"
  val FractionDigitsField: String = "fractionDigits"
  val TypeName: String = "centPrecision"

  def fromDecimalAmount(amount: BigDecimal, currency: Currency)(implicit
      mode: RoundingMode): Money =
    Money(amount.setScale(currency.getDefaultFractionDigits, mode), currency)

  val bdOne: BigDecimal = BigDecimal(1)
  val bdTen: BigDecimal = BigDecimal(10)

  private val centFactorZeroFractionDigit = bdOne / bdTen.pow(0)
  private val centFactorOneFractionDigit = bdOne / bdTen.pow(1)
  private val centFactorTwoFractionDigit = bdOne / bdTen.pow(2)
  private val centFactorThreeFractionDigit = bdOne / bdTen.pow(3)
  private val centFactorFourFractionDigit = bdOne / bdTen.pow(4)

  private[util] def cachedCentFactor(currencyFractionDigits: Int): BigDecimal =
    currencyFractionDigits match {
      case 0 => centFactorZeroFractionDigit
      case 1 => centFactorOneFractionDigit
      case 2 => centFactorTwoFractionDigit
      case 3 => centFactorThreeFractionDigit
      case 4 => centFactorFourFractionDigit
      case other => bdOne / bdTen.pow(other)
    }

  def fromCentAmount(centAmount: Long, currency: Currency): Money = {
    val currencyFractionDigits = currency.getDefaultFractionDigits
    val centFactor = cachedCentFactor(currencyFractionDigits)
    val amount = BigDecimal(centAmount) * centFactor

    fromDecimalAmount(amount, currency)(BigDecimal.RoundingMode.UNNECESSARY)
  }

  private val cachedZeroEUR = fromCentAmount(0L, Currency.getInstance("EUR"))
  private val cachedZeroUSD = fromCentAmount(0L, Currency.getInstance("USD"))
  private val cachedZeroGBP = fromCentAmount(0L, Currency.getInstance("GBP"))
  private val cachedZeroJPY = fromCentAmount(0L, Currency.getInstance("JPY"))

  def zero(currency: Currency): Money =
    currency.getCurrencyCode match {
      case "EUR" => cachedZeroEUR
      case "USD" => cachedZeroUSD
      case "GBP" => cachedZeroGBP
      case "JPY" => cachedZeroJPY
      case _ => fromCentAmount(0L, currency)
    }
}

case class HighPrecisionMoney private (
    amount: BigDecimal,
    fractionDigits: Int,
    centAmount: Long,
    currency: Currency)
    extends BaseMoney
    with Ordered[Money] {
  import HighPrecisionMoney._

  require(
    amount.scale == fractionDigits,
    "The scale of the given amount does not match the scale of the provided currency." +
      " - " + amount.scale + " <-> " + fractionDigits
  )

  require(
    fractionDigits >= currency.getDefaultFractionDigits,
    "`fractionDigits` should be  >= than the default fraction digits of the currency.")

  val `type`: String = TypeName

  lazy val preciseAmountAsLong: Long =
    (amount * Money.bdTen.pow(fractionDigits)).toLongExact // left side could be cached if necessary

  override def operation: HighPrecisionMoneyOperation =
    HighPrecisionMoneyOperation
      .fromHighPrecisionMoney(
        preciseAmountAsLong,
        fractionDigits,
        currency
      )

  def withFractionDigits(fd: Int)(implicit mode: RoundingMode): HighPrecisionMoney = {
    val newAmount = amount.setScale(fd, mode)

    HighPrecisionMoney(newAmount, fd, roundToCents(newAmount, currency), currency)
  }

  def updateCentAmountWithRoundingMode(implicit mode: RoundingMode): HighPrecisionMoney =
    copy(centAmount = roundToCents(amount, currency))

  def toMoneyWithPrecisionLoss: Money =
    Money.fromCentAmount(this.centAmount, currency)

  def compare(other: Money): Int = {
    BaseMoney.requireSameCurrency(this, other)

    this.amount.compare(other.amount)
  }

  override def toString: String =
    this.amount.bigDecimal.toPlainString + " " + this.currency.getCurrencyCode

  def toString(nf: NumberFormat, locale: Locale): String = {
    require(nf.getCurrency eq this.currency)

    nf.format(this.amount.doubleValue) + " " + this.currency.getSymbol(locale)
  }
}

object HighPrecisionMoney {
  object ImplicitsDecimal {
    final implicit class HighPrecisionMoneyNotation(val amount: BigDecimal) extends AnyVal {
      def EUR: HighPrecisionMoney = HighPrecisionMoney.EUR(amount)
      def USD: HighPrecisionMoney = HighPrecisionMoney.USD(amount)
      def GBP: HighPrecisionMoney = HighPrecisionMoney.GBP(amount)
      def JPY: HighPrecisionMoney = HighPrecisionMoney.JPY(amount)
    }
  }

  object ImplicitsDecimalPrecise {
    final implicit class HighPrecisionPreciseMoneyNotation(val amount: BigDecimal) extends AnyVal {
      def EUR_PRECISE(precision: Int): HighPrecisionMoney =
        HighPrecisionMoney.EUR(amount, Some(precision))
      def USD_PRECISE(precision: Int): HighPrecisionMoney =
        HighPrecisionMoney.EUR(amount, Some(precision))
      def GBP_PRECISE(precision: Int): HighPrecisionMoney =
        HighPrecisionMoney.EUR(amount, Some(precision))
      def JPY_PRECISE(precision: Int): HighPrecisionMoney =
        HighPrecisionMoney.EUR(amount, Some(precision))
    }
  }

  object ImplicitsString {
    implicit def stringMoneyNotation(amount: String): ImplicitsDecimal.HighPrecisionMoneyNotation =
      new ImplicitsDecimal.HighPrecisionMoneyNotation(BigDecimal(amount))
  }

  object ImplicitsStringPrecise {
    implicit def stringPreciseMoneyNotation(
        amount: String): ImplicitsDecimalPrecise.HighPrecisionPreciseMoneyNotation =
      new ImplicitsDecimalPrecise.HighPrecisionPreciseMoneyNotation(BigDecimal(amount))
  }

  def EUR(amount: BigDecimal, fractionDigits: Option[Int] = None): HighPrecisionMoney =
    simpleValueMeantToBeUsedOnlyInTests(amount, "EUR", fractionDigits)
  def USD(amount: BigDecimal, fractionDigits: Option[Int] = None): HighPrecisionMoney =
    simpleValueMeantToBeUsedOnlyInTests(amount, "USD", fractionDigits)
  def GBP(amount: BigDecimal, fractionDigits: Option[Int] = None): HighPrecisionMoney =
    simpleValueMeantToBeUsedOnlyInTests(amount, "GBP", fractionDigits)
  def JPY(amount: BigDecimal, fractionDigits: Option[Int] = None): HighPrecisionMoney =
    simpleValueMeantToBeUsedOnlyInTests(amount, "JPY", fractionDigits)

  val CurrencyCodeField: String = "currencyCode"
  val CentAmountField: String = "centAmount"
  val PreciseAmountField: String = "preciseAmount"
  val FractionDigitsField: String = "fractionDigits"

  val TypeName: String = "highPrecision"
  val MaxFractionDigits = 20

  private def simpleValueMeantToBeUsedOnlyInTests(
      amount: BigDecimal,
      currencyCode: String,
      fractionDigits: Option[Int]): HighPrecisionMoney = {
    val currency = Currency.getInstance(currencyCode)
    val fd = fractionDigits.getOrElse(currency.getDefaultFractionDigits)

    fromDecimalAmount(amount, fd, currency)(BigDecimal.RoundingMode.HALF_EVEN)
  }

  def roundToCents(amount: BigDecimal, currency: Currency)(implicit mode: RoundingMode): Long =
    (amount.setScale(currency.getDefaultFractionDigits, mode) / centFactor(currency)).toLong

  def sameScale(m1: HighPrecisionMoney, m2: HighPrecisionMoney): (BigDecimal, BigDecimal, Int) = {
    val newFractionDigits = math.max(m1.fractionDigits, m2.fractionDigits)

    def scale(m: HighPrecisionMoney, s: Int) =
      if (m.fractionDigits < s) m.amount.setScale(s)
      else if (m.fractionDigits == s) m.amount
      else throw new IllegalStateException("Downscale is not allowed/expected at this point!")

    (scale(m1, newFractionDigits), scale(m2, newFractionDigits), newFractionDigits)
  }

  def calc(
      m1: HighPrecisionMoney,
      m2: HighPrecisionMoney,
      fn: (BigDecimal, BigDecimal) => BigDecimal)(implicit
      mode: RoundingMode): HighPrecisionMoney = {
    BaseMoney.requireSameCurrency(m1, m2)

    val (a1, a2, fd) = sameScale(m1, m2)

    fromDecimalAmount(fn(a1, a2), fd, m1.currency)
  }

  def factor(fractionDigits: Int): BigDecimal = Money.cachedCentFactor(fractionDigits)
  def centFactor(currency: Currency): BigDecimal = factor(currency.getDefaultFractionDigits)

  def fromDecimalAmount(amount: BigDecimal, fractionDigits: Int, currency: Currency)(implicit
      mode: RoundingMode): HighPrecisionMoney = {
    val scaledAmount = amount.setScale(fractionDigits, mode)

    HighPrecisionMoney(scaledAmount, fractionDigits, roundToCents(scaledAmount, currency), currency)
  }

  def fromCentAmount(
      centAmount: Long,
      fractionDigits: Int,
      currency: Currency): HighPrecisionMoney = {
    val amount = BigDecimal(centAmount) * centFactor(currency)

    HighPrecisionMoney(
      amount.setScale(fractionDigits, BigDecimal.RoundingMode.UNNECESSARY),
      fractionDigits,
      centAmount,
      currency)
  }

  def zero(fractionDigits: Int, currency: Currency): HighPrecisionMoney =
    fromCentAmount(0L, fractionDigits, currency)

  /* centAmount provides an escape hatch in cases where the default rounding mode is not applicable */
  def fromPreciseAmount(
      preciseAmount: Long,
      fractionDigits: Int,
      currency: Currency,
      centAmount: Option[Long]): ValidatedNel[String, HighPrecisionMoney] =
    for {
      fd <- validateFractionDigits(fractionDigits, currency)
      amount = BigDecimal(preciseAmount) * factor(fd)
      scaledAmount = amount.setScale(fd, BigDecimal.RoundingMode.UNNECESSARY)
      ca <- validateCentAmount(scaledAmount, centAmount, currency)
      // TODO: revisit this part! the rounding mode might be dynamic and configured elsewhere
      actualCentAmount = ca.getOrElse(
        roundToCents(scaledAmount, currency)(BigDecimal.RoundingMode.HALF_EVEN))
    } yield HighPrecisionMoney(scaledAmount, fd, actualCentAmount, currency)

  private def validateFractionDigits(
      fractionDigits: Int,
      currency: Currency): ValidatedNel[String, Int] =
    if (fractionDigits <= currency.getDefaultFractionDigits)
      s"fractionDigits must be > ${currency.getDefaultFractionDigits} (default fraction digits defined by currency ${currency.getCurrencyCode}).".invalidNel
    else if (fractionDigits > MaxFractionDigits)
      s"fractionDigits must be <= $MaxFractionDigits.".invalidNel
    else
      fractionDigits.validNel

  private def validateCentAmount(
      amount: BigDecimal,
      centAmount: Option[Long],
      currency: Currency): ValidatedNel[String, Option[Long]] =
    centAmount match {
      case Some(actual) =>
        val min = roundToCents(amount, currency)(RoundingMode.FLOOR)
        val max = roundToCents(amount, currency)(RoundingMode.CEILING)

        if (actual < min || actual > max)
          s"centAmount must be correctly rounded preciseAmount (a number between $min and $max).".invalidNel
        else
          centAmount.validNel

      case _ =>
        centAmount.validNel
    }

  def fromMoney(money: Money, fractionDigits: Int): HighPrecisionMoney =
    HighPrecisionMoney(
      money.amount.setScale(fractionDigits, RoundingMode.UNNECESSARY),
      fractionDigits,
      money.centAmount,
      money.currency)
}
