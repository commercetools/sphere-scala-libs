package io.sphere.util

import java.math.MathContext
import java.text.NumberFormat
import java.util.Currency

import cats.Monoid

import scala.math._
import BigDecimal.RoundingMode._

/**
 * Represents an amount of money in a certain currency.
 *
 * This implementation does not support fractional money units (eg a tenth cent).
 * Amounts are always rounded to the nearest, smallest unit of the respective currency.
 * The rounding mode can be specified using an implicit `BigDecimal.RoundingMode`.
 *
 * @param amount The amount as a decimal value. The scale must be equal to or less than the
 *               number of fractional digits of the currency.
 * @param currency The currency of the amount.
 */
case class Money(amount: BigDecimal, currency: Currency) extends Ordered[Money] {
  import Money._

  require(amount.scale == currency.getDefaultFractionDigits,
          "The scale of the given amount does not match the scale of the provided currency." +
          " - " + amount.scale + " <-> " + currency.getDefaultFractionDigits)

  private val centFactor: Double = 1 / pow(10, currency.getDefaultFractionDigits)

  lazy val centAmount: Long = (amount / centFactor).toLong

  def withCentAmount(centAmount: Long): Money = {
    val newAmount = BigDecimal(centAmount) * centFactor
    copy(amount = newAmount.setScale(currency.getDefaultFractionDigits))
  }

  /**
   * Creates a new Money instance with the same currency and the amount conforming
   * to the given MathContext (scale and rounding mode).
   */
  def apply(mc: MathContext): Money = make(this.amount(mc), this.currency)

  def + (m: Money): Money = {
    requireSameCurrency(m)
    make(this.amount + m.amount, this.currency)
  }

  def + (m: BigDecimal): Money = this + make(m, this.currency)

  def - (m: Money): Money = {
    requireSameCurrency(m)
    make(this.amount - m.amount, this.currency)
  }

  def - (m: BigDecimal): Money = this - make(m, this.currency)

  def * (m: Money)(implicit mode: RoundingMode): Money = {
    requireSameCurrency(m)
    this * m.amount
  }

  def * (m: BigDecimal)(implicit mode: RoundingMode): Money = {
    make((this.amount * m).setScale(this.amount.scale, mode), this.currency)
  }

  /** Divide to integral value + remainder */
  def /% (m: BigDecimal)(implicit mode: RoundingMode): (Money, Money) = {
    val result_remainder = this.amount /% m
    ( make(result_remainder._1, this.currency)
    , make(result_remainder._2, this.currency) )
  }

  def % (m: Money)(implicit mode: RoundingMode): Money = this.remainder(m)

  def % (m: BigDecimal)(implicit mode: RoundingMode): Money = this.remainder(make(m, this.currency))

  def remainder(m: Money)(implicit mode: RoundingMode): Money = {
    requireSameCurrency(m)
    make(this.amount.remainder(m.amount), this.currency)
  }

  def remainder(m: BigDecimal)(implicit mode: RoundingMode): Money = this.remainder(make(m, this.currency))

  def unary_- = make(-this.amount, this.currency)

  /**
   * Partitions this amount of money into several parts where the size of the individual
   * parts are defined by the given ratios. The partitioning takes care of not losing or gaining
   * any money by distributing any remaining "cents" evenly across the partitions.
   *
   * <p>Example: (0.05 EUR) partition (3,7) == Seq(0.02 EUR, 0.03 EUR)</p>
   */
  def partition(ratios: Int*): Seq[Money] = {
    val total = ratios.sum
    val amountInCents = (this.amount / centFactor).toBigInt
    val amounts = Seq(ratios.map(amountInCents * _ / total):_*)
    var remainder = amounts.foldLeft(amountInCents)(_ - _)
    amounts.map(amount => {
      remainder -= 1
      make(BigDecimal(amount + (if (remainder >= 0) 1 else 0)) * centFactor, this.currency)
    })
  }

  def compare(that: Money): Int =  {
    requireSameCurrency(that)
    this.amount compare that.amount
  }

  override def toString = ("%." + this.currency.getDefaultFractionDigits + "f %s")
    .format(this.amount, this.currency.getSymbol)

  def toString(nf: NumberFormat) = {
    require(nf.getCurrency eq this.currency)
    nf.format(this.amount.doubleValue) + " " + this.currency.getSymbol
  }

  private def requireSameCurrency(m: Money): Unit =
    require(this.currency eq m.currency, s"${this.currency} != ${m.currency}")
}

object Money {
  final implicit class MoneyNotation(val amount: Double) extends AnyVal {
    def EUR = Money.EUR(amount)
    def USD = Money.USD(amount)
    def GBP = Money.GBP(amount)
    def JPY = Money.JPY(amount)
  }

  def EUR(amount: BigDecimal): Money = make(amount, Currency.getInstance("EUR"))
  def USD(amount: BigDecimal): Money = make(amount, Currency.getInstance("USD"))
  def GBP(amount: BigDecimal): Money = make(amount, Currency.getInstance("GBP"))
  def JPY(amount: BigDecimal): Money = make(amount, Currency.getInstance("JPY"))

  /** Creates a new Money object, forcing the scale according to the currency. */
  def make(amount: BigDecimal, currency: Currency, mode: RoundingMode): Money =
    Money(amount.setScale(currency.getDefaultFractionDigits, mode), currency)

  def make(amount: BigDecimal, currency: Currency): Money =
    make(amount, currency, BigDecimal.RoundingMode.HALF_EVEN)

  implicit def moneyMonoid(implicit c: Currency): Monoid[Money] = new Monoid[Money] {
    def combine(x: Money, y: Money): Money = x + y
    val empty: Money = Money.make(0, c)
  }

  implicit def moneyMonoid(implicit c: Currency, mode: RoundingMode): Monoid[Money] = new Monoid[Money] {
    def combine(x: Money, y: Money): Money = x + y
    val empty: Money = Money.make(0, c, mode)
  }
}