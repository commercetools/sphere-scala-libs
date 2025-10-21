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
import io.sphere.util.BaseMoney.bigDecimalToMoneyLong
import io.sphere.util.Money.ImplicitsDecimal.MoneyNotation
import cats.data.Validated
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class MoneyOverflowException extends RuntimeException("A Money operation resulted in an overflow.")

sealed trait BaseMoney {
  def `type`: String

  def currency: Currency

  // Use with CAUTION! will loose precision in case of a high precision money value
  def centAmount: Long

  /** Normalized representation.
    *
    * for centPrecision:
    *   - centAmount: 1234 EUR
    *   - amount: 12.34
    *
    * for highPrecision: preciseAmount:
    *   - 123456 EUR (with fractionDigits = 4)
    *   - amount: 12.3456
    */
  def amount: BigDecimal

  def fractionDigits: Int

  def toMoneyWithPrecisionLoss: Money

  def +(m: Money)(implicit mode: RoundingMode): BaseMoney
  def +(m: HighPrecisionMoney)(implicit mode: RoundingMode): BaseMoney
  def +(m: BaseMoney)(implicit mode: RoundingMode): BaseMoney
  def +(m: BigDecimal)(implicit mode: RoundingMode): BaseMoney

  def -(m: Money)(implicit mode: RoundingMode): BaseMoney
  def -(m: HighPrecisionMoney)(implicit mode: RoundingMode): BaseMoney
  def -(m: BaseMoney)(implicit mode: RoundingMode): BaseMoney
  def -(m: BigDecimal)(implicit mode: RoundingMode): BaseMoney

  def *(m: Money)(implicit mode: RoundingMode): BaseMoney
  def *(m: HighPrecisionMoney)(implicit mode: RoundingMode): BaseMoney
  def *(m: BaseMoney)(implicit mode: RoundingMode): BaseMoney
  def *(m: BigDecimal)(implicit mode: RoundingMode): BaseMoney
}

object BaseMoney {
  val TypeField: String = "type"

  def requireSameCurrency(m1: BaseMoney, m2: BaseMoney): Unit =
    require(m1.currency eq m2.currency, s"${m1.currency} != ${m2.currency}")

  def toScalaRoundingMode(mode: java.math.RoundingMode): RoundingMode =
    BigDecimal.RoundingMode(mode.ordinal())

  implicit def baseMoneyMonoid(implicit c: Currency, mode: RoundingMode): Monoid[BaseMoney] =
    new Monoid[BaseMoney] {
      def combine(x: BaseMoney, y: BaseMoney): BaseMoney = x + y
      val empty: BaseMoney = Money.zero(c)
    }

  private[util] def bigDecimalToMoneyLong(amount: BigDecimal): Long =
    try amount.toLongExact
    catch { case _: ArithmeticException => throw new MoneyOverflowException }
}

/** Represents an amount of money in a certain currency.
  *
  * This implementation does not support fractional money units (eg a tenth cent). Amounts are
  * always rounded to the nearest, smallest unit of the respective currency. The rounding mode can
  * be specified using an implicit `BigDecimal.RoundingMode`.
  *
  * @param centAmount
  *   The amount in the smallest indivisible unit of the respective currency represented as a single
  *   Long value.
  * @param currency
  *   The currency of the amount.
  */
case class Money private (centAmount: Long, currency: Currency)
    extends BaseMoney
    with Ordered[Money] {
  import Money._

  private val centFactor: Double = 1 / pow(10, currency.getDefaultFractionDigits)
  private val backwardsCompatibleRoundingModeForOperations = BigDecimal.RoundingMode.HALF_EVEN

  val `type`: String = TypeName

  override def fractionDigits: Int = currency.getDefaultFractionDigits
  override lazy val amount: BigDecimal = BigDecimal(centAmount) * cachedCentFactor(fractionDigits)

  def withCentAmount(centAmount: Long): Money =
    copy(centAmount = centAmount)

  def toHighPrecisionMoney(fractionDigits: Int): HighPrecisionMoney =
    HighPrecisionMoney.fromMoney(this, fractionDigits)

  /** Creates a new Money instance with the same currency and the amount conforming to the given
    * MathContext (scale and rounding mode).
    */
  def apply(mc: MathContext): Money =
    fromDecimalAmount(this.amount(mc), this.currency)(RoundingMode.HALF_EVEN)

  def +(m: Money)(implicit mode: RoundingMode): Money = {
    BaseMoney.requireSameCurrency(this, m)

    fromDecimalAmount(this.amount + m.amount, this.currency)(
      backwardsCompatibleRoundingModeForOperations)
  }

  def +(m: HighPrecisionMoney)(implicit mode: RoundingMode): HighPrecisionMoney =
    this.toHighPrecisionMoney(m.fractionDigits) + m

  def +(money: BaseMoney)(implicit mode: RoundingMode): BaseMoney = money match {
    case m: Money => this + m
    case m: HighPrecisionMoney => this + m
  }

  def +(m: BigDecimal)(implicit mode: RoundingMode): Money =
    this + fromDecimalAmount(m, this.currency)

  def -(m: Money)(implicit mode: RoundingMode): Money = {
    BaseMoney.requireSameCurrency(this, m)
    fromDecimalAmount(this.amount - m.amount, this.currency)
  }

  def -(money: BaseMoney)(implicit mode: RoundingMode): BaseMoney = money match {
    case m: Money => this - m
    case m: HighPrecisionMoney => this - m
  }

  def -(m: HighPrecisionMoney)(implicit mode: RoundingMode): HighPrecisionMoney =
    this.toHighPrecisionMoney(m.fractionDigits) - m

  def -(m: BigDecimal)(implicit mode: RoundingMode): Money =
    this - fromDecimalAmount(m, this.currency)

  def *(m: Money)(implicit mode: RoundingMode): Money = {
    BaseMoney.requireSameCurrency(this, m)
    this * m.amount
  }

  def *(m: HighPrecisionMoney)(implicit mode: RoundingMode): HighPrecisionMoney =
    this.toHighPrecisionMoney(m.fractionDigits) * m

  def *(money: BaseMoney)(implicit mode: RoundingMode): BaseMoney = money match {
    case m: Money => this * m
    case m: HighPrecisionMoney => this * m
  }

  def *(m: BigDecimal)(implicit mode: RoundingMode): Money =
    fromDecimalAmount((this.amount * m).setScale(this.amount.scale, mode), this.currency)

  /** Divide to integral value + remainder */
  def /%(m: BigDecimal)(implicit mode: RoundingMode): (Money, Money) = {
    val (result, remainder) = this.amount /% m

    (fromDecimalAmount(result, this.currency), fromDecimalAmount(remainder, this.currency))
  }

  def %(m: Money)(implicit mode: RoundingMode): Money = this.remainder(m)

  def %(m: BigDecimal)(implicit mode: RoundingMode): Money =
    this.remainder(fromDecimalAmount(m, this.currency))

  def remainder(m: Money)(implicit mode: RoundingMode): Money = {
    BaseMoney.requireSameCurrency(this, m)

    fromDecimalAmount(this.amount.remainder(m.amount), this.currency)
  }

  def remainder(m: BigDecimal)(implicit mode: RoundingMode): Money =
    this.remainder(fromDecimalAmount(m, this.currency)(RoundingMode.HALF_EVEN))

  def unary_- : Money =
    fromDecimalAmount(-this.amount, this.currency)(BigDecimal.RoundingMode.UNNECESSARY)

  /** Partitions this amount of money into several parts where the size of the individual parts are
    * defined by the given ratios. The partitioning takes care of not losing or gaining any money by
    * distributing any remaining "cents" evenly across the partitions.
    *
    * <p>Example: (0.05 EUR) partition (3,7) == Seq(0.02 EUR, 0.03 EUR)</p>
    */
  def partition(ratios: Int*): Seq[Money] = {
    val total = ratios.sum
    val amountInCents = BigInt(this.centAmount)
    val amounts = ratios.map(amountInCents * _ / total)
    var remainder = amounts.foldLeft(amountInCents)(_ - _)
    amounts.map { amount =>
      remainder -= 1
      fromDecimalAmount(
        BigDecimal(amount + (if (remainder >= 0) 1 else 0)) * centFactor,
        this.currency)(backwardsCompatibleRoundingModeForOperations)
    }
  }

  def toMoneyWithPrecisionLoss: Money = this

  def compare(that: Money): Int = {
    BaseMoney.requireSameCurrency(this, that)
    this.centAmount.compare(that.centAmount)
  }

  override def toString: String = Money.toString(centAmount, fractionDigits, currency)

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

  final val CurrencyCodeField: String = "currencyCode"
  final val CentAmountField: String = "centAmount"
  final val FractionDigitsField: String = "fractionDigits"
  final val TypeName: String = "centPrecision"

  def fromDecimalAmount(amount: BigDecimal, currency: Currency)(implicit
      mode: RoundingMode): Money = {
    val fractionDigits = currency.getDefaultFractionDigits
    val centAmountBigDecimal = amount * cachedCentPower(fractionDigits)
    val centAmountBigDecimalZeroScale = centAmountBigDecimal.setScale(0, mode)
    Money(bigDecimalToMoneyLong(centAmountBigDecimalZeroScale), currency)
  }

  def apply(amount: BigDecimal, currency: Currency): Validated[Throwable, Money] =
    Try(unsafeApply(amount, currency)) match {
      case Failure(exception) => exception.invalid
      case Success(value) => value.valid
    }

  def unsafeApply(amount: BigDecimal, currency: Currency): Money = {
    require(
      amount.scale == currency.getDefaultFractionDigits,
      "The scale of the given amount does not match the scale of the provided currency." +
        " - " + amount.scale + " <-> " + currency.getDefaultFractionDigits
    )
    fromDecimalAmount(amount, currency)(BigDecimal.RoundingMode.UNNECESSARY)
  }

  def unsafeApply(centAmount: Long, currency: Currency): Money = new Money(centAmount, currency)

  private final val bdOne: BigDecimal = BigDecimal(1)
  final val bdTen: BigDecimal = BigDecimal(10)

  private final val centPowerZeroFractionDigit = bdOne
  private final val centPowerOneFractionDigit = bdTen
  private final val centPowerTwoFractionDigit = bdTen.pow(2)
  private final val centPowerThreeFractionDigit = bdTen.pow(3)
  private final val centPowerFourFractionDigit = bdTen.pow(4)

  private[util] def cachedCentPower(currencyFractionDigits: Int): BigDecimal =
    currencyFractionDigits match {
      case 0 => centPowerZeroFractionDigit
      case 1 => centPowerOneFractionDigit
      case 2 => centPowerTwoFractionDigit
      case 3 => centPowerThreeFractionDigit
      case 4 => centPowerFourFractionDigit
      case other => bdTen.pow(other)
    }

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

  def fromCentAmount(centAmount: Long, currency: Currency): Money =
    new Money(centAmount, currency)

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

  implicit def moneyMonoid(implicit c: Currency, mode: RoundingMode): Monoid[Money] =
    new Monoid[Money] {
      def combine(x: Money, y: Money): Money = x + y
      val empty: Money = Money.zero(c)
    }

  def toString(amount: Long, fractionDigits: Int, currency: Currency): String = {
    val amountDigits = amount.toString.toList
    val leadingZerosLength = fractionDigits - amountDigits.length + 1
    val leadingZeros = List.fill(leadingZerosLength)('0')
    val allDigits = leadingZeros ::: amountDigits
    val radixPosition = allDigits.length - fractionDigits
    val (integer, fractional) = allDigits.splitAt(radixPosition)
    if (fractional.nonEmpty)
      s"${integer.mkString}.${fractional.mkString} ${currency.getCurrencyCode}"
    else
      s"${integer.mkString} ${currency.getCurrencyCode}"
  }
}

case class HighPrecisionMoney private (
    preciseAmount: Long,
    fractionDigits: Int,
    centAmount: Long,
    currency: Currency)
    extends BaseMoney
    with Ordered[Money] {
  import HighPrecisionMoney._

  require(
    fractionDigits >= currency.getDefaultFractionDigits,
    "`fractionDigits` should be  >= than the default fraction digits of the currency.")

  val `type`: String = TypeName

  lazy val amount: BigDecimal =
    (BigDecimal(preciseAmount) * factor(fractionDigits)).setScale(fractionDigits)

  def withFractionDigits(fd: Int)(implicit mode: RoundingMode): HighPrecisionMoney = {
    val scaledAmount = amount.setScale(fd, mode)
    val newCentAmount = roundToCents(scaledAmount, currency)
    HighPrecisionMoney(amountToPreciseAmount(scaledAmount, fd), fd, newCentAmount, currency)
  }

  def updateCentAmountWithRoundingMode(implicit mode: RoundingMode): HighPrecisionMoney =
    copy(centAmount = roundToCents(amount, currency))

  def +(other: HighPrecisionMoney)(implicit mode: RoundingMode): HighPrecisionMoney =
    calc(this, other, _ + _)

  def +(m: Money)(implicit mode: RoundingMode): HighPrecisionMoney =
    this + m.toHighPrecisionMoney(fractionDigits)

  def +(money: BaseMoney)(implicit mode: RoundingMode): HighPrecisionMoney = money match {
    case m: Money => this + m
    case m: HighPrecisionMoney => this + m
  }

  def +(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoney =
    this + fromDecimalAmount(other, this.fractionDigits, this.currency)

  def -(other: HighPrecisionMoney)(implicit mode: RoundingMode): HighPrecisionMoney =
    calc(this, other, _ - _)

  def -(m: Money)(implicit mode: RoundingMode): HighPrecisionMoney =
    this - m.toHighPrecisionMoney(fractionDigits)

  def -(money: BaseMoney)(implicit mode: RoundingMode): HighPrecisionMoney = money match {
    case m: Money => this - m
    case m: HighPrecisionMoney => this - m
  }

  def -(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoney =
    this - fromDecimalAmount(other, this.fractionDigits, this.currency)

  def *(other: HighPrecisionMoney)(implicit mode: RoundingMode): HighPrecisionMoney =
    calc(this, other, _ * _)

  def *(m: Money)(implicit mode: RoundingMode): HighPrecisionMoney =
    this * m.toHighPrecisionMoney(fractionDigits)

  def *(money: BaseMoney)(implicit mode: RoundingMode): HighPrecisionMoney = money match {
    case m: Money => this * m
    case m: HighPrecisionMoney => this * m
  }

  def *(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoney =
    this * fromDecimalAmount(other, this.fractionDigits, this.currency)

  /** Divide to integral value + remainder */
  def /%(m: BigDecimal)(implicit mode: RoundingMode): (HighPrecisionMoney, HighPrecisionMoney) = {
    val (result, remainder) = this.amount /% m

    fromDecimalAmount(result, fractionDigits, this.currency) ->
      fromDecimalAmount(remainder, fractionDigits, this.currency)
  }

  def %(other: HighPrecisionMoney)(implicit mode: RoundingMode): HighPrecisionMoney =
    this.remainder(other)

  def %(m: Money)(implicit mode: RoundingMode): HighPrecisionMoney =
    this.remainder(m.toHighPrecisionMoney(fractionDigits))

  def %(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoney =
    this.remainder(fromDecimalAmount(other, this.fractionDigits, this.currency))

  def remainder(other: HighPrecisionMoney)(implicit mode: RoundingMode): HighPrecisionMoney =
    calc(this, other, _ remainder _)

  def remainder(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoney =
    this.remainder(fromDecimalAmount(other, this.fractionDigits, this.currency))

  def unary_- : HighPrecisionMoney =
    fromDecimalAmount(-this.amount, this.fractionDigits, this.currency)(
      BigDecimal.RoundingMode.UNNECESSARY)

  /** Partitions this amount of money into several parts where the size of the individual parts are
    * defined by the given ratios. The partitioning takes care of not losing or gaining any money by
    * distributing any remaining "cents" evenly across the partitions.
    *
    * <p>Example: (0.05 EUR) partition (3,7) == Seq(0.02 EUR, 0.03 EUR)</p>
    */
  def partition(ratios: Int*)(implicit mode: RoundingMode): Seq[HighPrecisionMoney] = {
    val total = ratios.sum
    val factor = Money.cachedCentFactor(fractionDigits)
    val amountAsInt = BigInt(this.preciseAmount)
    val portionAmounts = ratios.map(amountAsInt * _ / total)
    var remainder = portionAmounts.foldLeft(amountAsInt)(_ - _)

    portionAmounts.map { portionAmount =>
      remainder -= 1

      fromDecimalAmount(
        BigDecimal(portionAmount + (if (remainder >= 0) 1 else 0)) * factor,
        this.fractionDigits,
        this.currency)
    }
  }

  def toMoneyWithPrecisionLoss: Money =
    Money.fromCentAmount(this.centAmount, currency)

  def compare(other: Money): Int = {
    BaseMoney.requireSameCurrency(this, other)

    this.amount.compare(other.amount)
  }

  override def toString: String = Money.toString(preciseAmount, fractionDigits, currency)

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
        HighPrecisionMoney.USD(amount, Some(precision))
      def GBP_PRECISE(precision: Int): HighPrecisionMoney =
        HighPrecisionMoney.GBP(amount, Some(precision))
      def JPY_PRECISE(precision: Int): HighPrecisionMoney =
        HighPrecisionMoney.JPY(amount, Some(precision))
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
    bigDecimalToMoneyLong(
      amount.setScale(currency.getDefaultFractionDigits, mode) / centFactor(currency))

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

  private def amountToPreciseAmount(amount: BigDecimal, fractionDigits: Int): Long =
    bigDecimalToMoneyLong(amount * Money.cachedCentPower(fractionDigits))

  def fromDecimalAmount(amount: BigDecimal, fractionDigits: Int, currency: Currency)(implicit
      mode: RoundingMode): HighPrecisionMoney = {
    val scaledAmount = amount.setScale(fractionDigits, mode)
    val preciseAmount = amountToPreciseAmount(scaledAmount, fractionDigits)
    val newCentAmount = roundToCents(scaledAmount, currency)
    HighPrecisionMoney(preciseAmount, fractionDigits, newCentAmount, currency)
  }

  private def centToPreciseAmount(
      centAmount: Long,
      fractionDigits: Int,
      currency: Currency): Long = {
    val centDigits = fractionDigits - currency.getDefaultFractionDigits
    if (centDigits >= 19)
      throw new IllegalArgumentException("Cannot represent number bigger than 10^19 with a Long")
    else
      Math.pow(10, centDigits).toLong * centAmount
  }

  def fromCentAmount(
      centAmount: Long,
      fractionDigits: Int,
      currency: Currency): HighPrecisionMoney =
    HighPrecisionMoney(
      centToPreciseAmount(centAmount, fractionDigits, currency),
      fractionDigits,
      centAmount,
      currency)

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
    } yield HighPrecisionMoney(preciseAmount, fd, actualCentAmount, currency)

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
      centToPreciseAmount(money.centAmount, fractionDigits, money.currency),
      fractionDigits,
      money.centAmount,
      money.currency)

  def monoid(fractionDigits: Int, c: Currency)(implicit
      mode: RoundingMode): Monoid[HighPrecisionMoney] = new Monoid[HighPrecisionMoney] {
    def combine(x: HighPrecisionMoney, y: HighPrecisionMoney): HighPrecisionMoney = x + y
    val empty: HighPrecisionMoney = HighPrecisionMoney.zero(fractionDigits, c)
  }
}
