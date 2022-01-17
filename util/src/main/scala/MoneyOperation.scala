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

sealed trait BaseMoneyOperation {
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

  def toMoneyOperationWithPrecisionLoss: MoneyOperation

  def +(m: MoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def +(m: HighPrecisionMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def +(m: BaseMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def +(m: BigDecimal)(implicit mode: RoundingMode): BaseMoneyOperation

  def -(m: MoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def -(m: HighPrecisionMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def -(m: BaseMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def -(m: BigDecimal)(implicit mode: RoundingMode): BaseMoneyOperation

  def *(m: MoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def *(m: HighPrecisionMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def *(m: BaseMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation
  def *(m: BigDecimal)(implicit mode: RoundingMode): BaseMoneyOperation
}

object BaseMoneyOperation {
  val TypeField: String = "type"

  def requireSameCurrency(m1: BaseMoneyOperation, m2: BaseMoneyOperation): Unit =
    require(m1.currency eq m2.currency, s"${m1.currency} != ${m2.currency}")

  def toScalaRoundingMode(mode: java.math.RoundingMode): RoundingMode =
    BigDecimal.RoundingMode(mode.ordinal())

  implicit def baseMoneyOperationMonoid(implicit
      c: Currency,
      mode: RoundingMode): Monoid[BaseMoneyOperation] =
    new Monoid[BaseMoneyOperation] {
      def combine(x: BaseMoneyOperation, y: BaseMoneyOperation): BaseMoneyOperation = x + y
      val empty: BaseMoneyOperation = MoneyOperation.zero(c)
    }
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
case class MoneyOperation private (amount: BigDecimal, currency: Currency)
    extends BaseMoneyOperation
    with Ordered[MoneyOperation] {
  import MoneyOperation._

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

  def withCentAmount(centAmount: Long): MoneyOperation = {
    val newAmount = BigDecimal(centAmount) * centFactor
    copy(amount = newAmount.setScale(currency.getDefaultFractionDigits))
  }

  def toHighPrecisionMoneyOperation(fractionDigits: Int): HighPrecisionMoneyOperation =
    HighPrecisionMoneyOperation.fromMoney(this, fractionDigits)

  /** Creates a new Money instance with the same currency and the amount conforming to the given
    * MathContext (scale and rounding mode).
    */
  def apply(mc: MathContext): MoneyOperation =
    fromDecimalAmount(this.amount(mc), this.currency)(RoundingMode.HALF_EVEN)

  def +(m: MoneyOperation)(implicit mode: RoundingMode): MoneyOperation = {
    BaseMoneyOperation.requireSameCurrency(this, m)

    fromDecimalAmount(this.amount + m.amount, this.currency)(
      backwardsCompatibleRoundingModeForOperations)
  }

  def +(m: HighPrecisionMoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this.toHighPrecisionMoneyOperation(m.fractionDigits) + m

  def +(money: BaseMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation = money match {
    case m: MoneyOperation => this + m
    case m: HighPrecisionMoneyOperation => this + m
  }

  def +(m: BigDecimal)(implicit mode: RoundingMode): MoneyOperation =
    this + fromDecimalAmount(m, this.currency)

  def -(m: MoneyOperation)(implicit mode: RoundingMode): MoneyOperation = {
    BaseMoneyOperation.requireSameCurrency(this, m)
    fromDecimalAmount(this.amount - m.amount, this.currency)
  }

  def -(money: BaseMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation = money match {
    case m: MoneyOperation => this - m
    case m: HighPrecisionMoneyOperation => this - m
  }

  def -(m: HighPrecisionMoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this.toHighPrecisionMoneyOperation(m.fractionDigits) - m

  def -(m: BigDecimal)(implicit mode: RoundingMode): MoneyOperation =
    this - fromDecimalAmount(m, this.currency)

  def *(m: MoneyOperation)(implicit mode: RoundingMode): MoneyOperation = {
    BaseMoneyOperation.requireSameCurrency(this, m)
    this * m.amount
  }

  def *(m: HighPrecisionMoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this.toHighPrecisionMoneyOperation(m.fractionDigits) * m

  def *(money: BaseMoneyOperation)(implicit mode: RoundingMode): BaseMoneyOperation = money match {
    case m: MoneyOperation => this * m
    case m: HighPrecisionMoneyOperation => this * m
  }

  def *(m: BigDecimal)(implicit mode: RoundingMode): MoneyOperation =
    fromDecimalAmount((this.amount * m).setScale(this.amount.scale, mode), this.currency)

  /** Divide to integral value + remainder */
  def /%(m: BigDecimal)(implicit mode: RoundingMode): (MoneyOperation, MoneyOperation) = {
    val (result, remainder) = this.amount /% m

    (fromDecimalAmount(result, this.currency), fromDecimalAmount(remainder, this.currency))
  }

  def %(m: MoneyOperation)(implicit mode: RoundingMode): MoneyOperation = this.remainder(m)

  def %(m: BigDecimal)(implicit mode: RoundingMode): MoneyOperation =
    this.remainder(fromDecimalAmount(m, this.currency))

  def remainder(m: MoneyOperation)(implicit mode: RoundingMode): MoneyOperation = {
    BaseMoneyOperation.requireSameCurrency(this, m)

    fromDecimalAmount(this.amount.remainder(m.amount), this.currency)
  }

  def remainder(m: BigDecimal)(implicit mode: RoundingMode): MoneyOperation =
    this.remainder(fromDecimalAmount(m, this.currency)(RoundingMode.HALF_EVEN))

  def unary_- : MoneyOperation =
    fromDecimalAmount(-this.amount, this.currency)(BigDecimal.RoundingMode.UNNECESSARY)

  /** Partitions this amount of money into several parts where the size of the individual parts are
    * defined by the given ratios. The partitioning takes care of not losing or gaining any money by
    * distributing any remaining "cents" evenly across the partitions.
    *
    * <p>Example: (0.05 EUR) partition (3,7) == Seq(0.02 EUR, 0.03 EUR)</p>
    */
  def partition(ratios: Int*): Seq[MoneyOperation] = {
    val total = ratios.sum
    val amountInCents = (this.amount / centFactor).toBigInt
    val amounts = ratios.map(amountInCents * _ / total)
    var remainder = amounts.foldLeft(amountInCents)(_ - _)
    amounts.map { amount =>
      remainder -= 1
      fromDecimalAmount(
        BigDecimal(amount + (if (remainder >= 0) 1 else 0)) * centFactor,
        this.currency)(backwardsCompatibleRoundingModeForOperations)
    }
  }

  def toMoneyOperationWithPrecisionLoss: MoneyOperation = this

  def compare(that: MoneyOperation): Int = {
    BaseMoneyOperation.requireSameCurrency(this, that)
    this.amount.compare(that.amount)
  }

  override def toString: String =
    this.amount.bigDecimal.toPlainString + " " + this.currency.getCurrencyCode

  def toString(nf: NumberFormat, locale: Locale): String = {
    require(nf.getCurrency eq this.currency)
    nf.format(this.amount.doubleValue) + " " + this.currency.getSymbol(locale)
  }
}

object MoneyOperation {
  object ImplicitsDecimal {
    final implicit class MoneyNotation(val amount: BigDecimal) extends AnyVal {
      def EUR: MoneyOperation = MoneyOperation.EUR(amount)
      def USD: MoneyOperation = MoneyOperation.USD(amount)
      def GBP: MoneyOperation = MoneyOperation.GBP(amount)
      def JPY: MoneyOperation = MoneyOperation.JPY(amount)
    }

    implicit def doubleMoneyNotation(amount: Double): MoneyNotation =
      new ImplicitsDecimal.MoneyNotation(BigDecimal(amount))
  }

  object ImplicitsString {
    implicit def stringMoneyNotation(
        amount: String): MoneyOperation.ImplicitsDecimal.MoneyNotation =
      new ImplicitsDecimal.MoneyNotation(BigDecimal(amount))
  }

  private def decimalAmountWithCurrencyAndHalfEvenRounding(amount: BigDecimal, currency: String) =
    fromDecimalAmount(amount, Currency.getInstance(currency))(BigDecimal.RoundingMode.HALF_EVEN)

  def EUR(amount: BigDecimal): MoneyOperation =
    decimalAmountWithCurrencyAndHalfEvenRounding(amount, "EUR")
  def USD(amount: BigDecimal): MoneyOperation =
    decimalAmountWithCurrencyAndHalfEvenRounding(amount, "USD")
  def GBP(amount: BigDecimal): MoneyOperation =
    decimalAmountWithCurrencyAndHalfEvenRounding(amount, "GBP")
  def JPY(amount: BigDecimal): MoneyOperation =
    decimalAmountWithCurrencyAndHalfEvenRounding(amount, "JPY")

  val CurrencyCodeField: String = "currencyCode"
  val CentAmountField: String = "centAmount"
  val FractionDigitsField: String = "fractionDigits"
  val TypeName: String = "centPrecision"

  def fromDecimalAmount(amount: BigDecimal, currency: Currency)(implicit
      mode: RoundingMode): MoneyOperation =
    MoneyOperation(amount.setScale(currency.getDefaultFractionDigits, mode), currency)

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

  def fromCentAmount(centAmount: Long, currency: Currency): MoneyOperation = {
    val currencyFractionDigits = currency.getDefaultFractionDigits
    val centFactor = cachedCentFactor(currencyFractionDigits)
    val amount = BigDecimal(centAmount) * centFactor

    fromDecimalAmount(amount, currency)(BigDecimal.RoundingMode.UNNECESSARY)
  }

  private val cachedZeroEUR = fromCentAmount(0L, Currency.getInstance("EUR"))
  private val cachedZeroUSD = fromCentAmount(0L, Currency.getInstance("USD"))
  private val cachedZeroGBP = fromCentAmount(0L, Currency.getInstance("GBP"))
  private val cachedZeroJPY = fromCentAmount(0L, Currency.getInstance("JPY"))

  def zero(currency: Currency): MoneyOperation =
    currency.getCurrencyCode match {
      case "EUR" => cachedZeroEUR
      case "USD" => cachedZeroUSD
      case "GBP" => cachedZeroGBP
      case "JPY" => cachedZeroJPY
      case _ => fromCentAmount(0L, currency)
    }

  implicit def moneyOperationMonoid(implicit
      c: Currency,
      mode: RoundingMode): Monoid[MoneyOperation] =
    new Monoid[MoneyOperation] {
      def combine(x: MoneyOperation, y: MoneyOperation): MoneyOperation = x + y
      val empty: MoneyOperation = MoneyOperation.zero(c)
    }
}

case class HighPrecisionMoneyOperation private (
    amount: BigDecimal,
    fractionDigits: Int,
    centAmount: Long,
    currency: Currency)
    extends BaseMoneyOperation
    with Ordered[MoneyOperation] {
  import HighPrecisionMoneyOperation._

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

  def withFractionDigits(fd: Int)(implicit mode: RoundingMode): HighPrecisionMoneyOperation = {
    val newAmount = amount.setScale(fd, mode)

    HighPrecisionMoneyOperation(newAmount, fd, roundToCents(newAmount, currency), currency)
  }

  def updateCentAmountWithRoundingMode(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    copy(centAmount = roundToCents(amount, currency))

  def +(other: HighPrecisionMoneyOperation)(implicit
      mode: RoundingMode): HighPrecisionMoneyOperation =
    calc(this, other, _ + _)

  def +(m: MoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this + m.toHighPrecisionMoneyOperation(fractionDigits)

  def +(money: BaseMoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    money match {
      case m: MoneyOperation => this + m
      case m: HighPrecisionMoneyOperation => this + m
    }

  def +(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this + fromDecimalAmount(other, this.fractionDigits, this.currency)

  def -(other: HighPrecisionMoneyOperation)(implicit
      mode: RoundingMode): HighPrecisionMoneyOperation =
    calc(this, other, _ - _)

  def -(m: MoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this - m.toHighPrecisionMoneyOperation(fractionDigits)

  def -(money: BaseMoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    money match {
      case m: MoneyOperation => this - m
      case m: HighPrecisionMoneyOperation => this - m
    }

  def -(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this - fromDecimalAmount(other, this.fractionDigits, this.currency)

  def *(other: HighPrecisionMoneyOperation)(implicit
      mode: RoundingMode): HighPrecisionMoneyOperation =
    calc(this, other, _ * _)

  def *(m: MoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this * m.toHighPrecisionMoneyOperation(fractionDigits)

  def *(money: BaseMoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    money match {
      case m: MoneyOperation => this * m
      case m: HighPrecisionMoneyOperation => this * m
    }

  def *(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this * fromDecimalAmount(other, this.fractionDigits, this.currency)

  /** Divide to integral value + remainder */
  def /%(m: BigDecimal)(implicit
      mode: RoundingMode): (HighPrecisionMoneyOperation, HighPrecisionMoneyOperation) = {
    val (result, remainder) = this.amount /% m

    fromDecimalAmount(result, fractionDigits, this.currency) ->
      fromDecimalAmount(remainder, fractionDigits, this.currency)
  }

  def %(other: HighPrecisionMoneyOperation)(implicit
      mode: RoundingMode): HighPrecisionMoneyOperation =
    this.remainder(other)

  def %(m: MoneyOperation)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this.remainder(m.toHighPrecisionMoneyOperation(fractionDigits))

  def %(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this.remainder(fromDecimalAmount(other, this.fractionDigits, this.currency))

  def remainder(other: HighPrecisionMoneyOperation)(implicit
      mode: RoundingMode): HighPrecisionMoneyOperation =
    calc(this, other, _ remainder _)

  def remainder(other: BigDecimal)(implicit mode: RoundingMode): HighPrecisionMoneyOperation =
    this.remainder(fromDecimalAmount(other, this.fractionDigits, this.currency))

  def unary_- : HighPrecisionMoneyOperation =
    fromDecimalAmount(-this.amount, this.fractionDigits, this.currency)(
      BigDecimal.RoundingMode.UNNECESSARY)

  /** Partitions this amount of money into several parts where the size of the individual parts are
    * defined by the given ratios. The partitioning takes care of not losing or gaining any money by
    * distributing any remaining "cents" evenly across the partitions.
    *
    * <p>Example: (0.05 EUR) partition (3,7) == Seq(0.02 EUR, 0.03 EUR)</p>
    */
  def partition(ratios: Int*)(implicit mode: RoundingMode): Seq[HighPrecisionMoneyOperation] = {
    val total = ratios.sum
    val factor = Money.cachedCentFactor(fractionDigits)
    val amountAsInt = (this.amount / factor).toBigInt
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

  def toMoneyOperationWithPrecisionLoss: MoneyOperation =
    MoneyOperation.fromCentAmount(this.centAmount, currency)

  def compare(other: MoneyOperation): Int = {
    BaseMoneyOperation.requireSameCurrency(this, other)

    this.amount.compare(other.amount)
  }

  override def toString: String =
    this.amount.bigDecimal.toPlainString + " " + this.currency.getCurrencyCode

  def toString(nf: NumberFormat, locale: Locale): String = {
    require(nf.getCurrency eq this.currency)

    nf.format(this.amount.doubleValue) + " " + this.currency.getSymbol(locale)
  }
}

object HighPrecisionMoneyOperation {
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
      def EUR_PRECISE(precision: Int): HighPrecisionMoneyOperation =
        HighPrecisionMoneyOperation.EUR(amount, Some(precision))
      def USD_PRECISE(precision: Int): HighPrecisionMoneyOperation =
        HighPrecisionMoneyOperation.EUR(amount, Some(precision))
      def GBP_PRECISE(precision: Int): HighPrecisionMoneyOperation =
        HighPrecisionMoneyOperation.EUR(amount, Some(precision))
      def JPY_PRECISE(precision: Int): HighPrecisionMoneyOperation =
        HighPrecisionMoneyOperation.EUR(amount, Some(precision))
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

  def EUR(amount: BigDecimal, fractionDigits: Option[Int] = None): HighPrecisionMoneyOperation =
    simpleValueMeantToBeUsedOnlyInTests(amount, "EUR", fractionDigits)
  def USD(amount: BigDecimal, fractionDigits: Option[Int] = None): HighPrecisionMoneyOperation =
    simpleValueMeantToBeUsedOnlyInTests(amount, "USD", fractionDigits)
  def GBP(amount: BigDecimal, fractionDigits: Option[Int] = None): HighPrecisionMoneyOperation =
    simpleValueMeantToBeUsedOnlyInTests(amount, "GBP", fractionDigits)
  def JPY(amount: BigDecimal, fractionDigits: Option[Int] = None): HighPrecisionMoneyOperation =
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
      fractionDigits: Option[Int]): HighPrecisionMoneyOperation = {
    val currency = Currency.getInstance(currencyCode)
    val fd = fractionDigits.getOrElse(currency.getDefaultFractionDigits)

    fromDecimalAmount(amount, fd, currency)(BigDecimal.RoundingMode.HALF_EVEN)
  }

  def roundToCents(amount: BigDecimal, currency: Currency)(implicit mode: RoundingMode): Long =
    (amount.setScale(currency.getDefaultFractionDigits, mode) / centFactor(currency)).toLong

  def sameScale(
      m1: HighPrecisionMoneyOperation,
      m2: HighPrecisionMoneyOperation): (BigDecimal, BigDecimal, Int) = {
    val newFractionDigits = math.max(m1.fractionDigits, m2.fractionDigits)

    def scale(m: HighPrecisionMoneyOperation, s: Int) =
      if (m.fractionDigits < s) m.amount.setScale(s)
      else if (m.fractionDigits == s) m.amount
      else throw new IllegalStateException("Downscale is not allowed/expected at this point!")

    (scale(m1, newFractionDigits), scale(m2, newFractionDigits), newFractionDigits)
  }

  def calc(
      m1: HighPrecisionMoneyOperation,
      m2: HighPrecisionMoneyOperation,
      fn: (BigDecimal, BigDecimal) => BigDecimal)(implicit
      mode: RoundingMode): HighPrecisionMoneyOperation = {
    BaseMoneyOperation.requireSameCurrency(m1, m2)

    val (a1, a2, fd) = sameScale(m1, m2)

    fromDecimalAmount(fn(a1, a2), fd, m1.currency)
  }

  def factor(fractionDigits: Int): BigDecimal = MoneyOperation.cachedCentFactor(fractionDigits)
  def centFactor(currency: Currency): BigDecimal = factor(currency.getDefaultFractionDigits)

  def fromDecimalAmount(amount: BigDecimal, fractionDigits: Int, currency: Currency)(implicit
      mode: RoundingMode): HighPrecisionMoneyOperation = {
    val scaledAmount = amount.setScale(fractionDigits, mode)

    HighPrecisionMoneyOperation(
      scaledAmount,
      fractionDigits,
      roundToCents(scaledAmount, currency),
      currency)
  }

  def fromCentAmount(
      centAmount: Long,
      fractionDigits: Int,
      currency: Currency): HighPrecisionMoneyOperation = {
    val amount = BigDecimal(centAmount) * centFactor(currency)

    HighPrecisionMoneyOperation(
      amount.setScale(fractionDigits, BigDecimal.RoundingMode.UNNECESSARY),
      fractionDigits,
      centAmount,
      currency)
  }

  def zero(fractionDigits: Int, currency: Currency): HighPrecisionMoneyOperation =
    fromCentAmount(0L, fractionDigits, currency)

  /* centAmount provides an escape hatch in cases where the default rounding mode is not applicable */
  def fromPreciseAmount(
      preciseAmount: Long,
      fractionDigits: Int,
      currency: Currency,
      centAmount: Option[Long]): ValidatedNel[String, HighPrecisionMoneyOperation] =
    for {
      fd <- validateFractionDigits(fractionDigits, currency)
      amount = BigDecimal(preciseAmount) * factor(fd)
      scaledAmount = amount.setScale(fd, BigDecimal.RoundingMode.UNNECESSARY)
      ca <- validateCentAmount(scaledAmount, centAmount, currency)
      // TODO: revisit this part! the rounding mode might be dynamic and configured elsewhere
      actualCentAmount = ca.getOrElse(
        roundToCents(scaledAmount, currency)(BigDecimal.RoundingMode.HALF_EVEN))
    } yield HighPrecisionMoneyOperation(scaledAmount, fd, actualCentAmount, currency)

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

  def fromMoney(money: MoneyOperation, fractionDigits: Int): HighPrecisionMoneyOperation =
    HighPrecisionMoneyOperation(
      money.amount.setScale(fractionDigits, RoundingMode.UNNECESSARY),
      fractionDigits,
      money.centAmount,
      money.currency)

  def monoid(fractionDigits: Int, c: Currency)(implicit
      mode: RoundingMode): Monoid[HighPrecisionMoneyOperation] =
    new Monoid[HighPrecisionMoneyOperation] {
      def combine(
          x: HighPrecisionMoneyOperation,
          y: HighPrecisionMoneyOperation): HighPrecisionMoneyOperation = x + y
      val empty: HighPrecisionMoneyOperation = HighPrecisionMoneyOperation.zero(fractionDigits, c)
    }
}
