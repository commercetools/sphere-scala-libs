package io.sphere.util

import java.util.Currency
import scala.annotation.tailrec

/** This object contains rounding algorithms for the Money classes. So far we used BigDecimal for
  * this purpose, but BigDecimal is slower and consumes more memory than this approach.
  */
object MoneyRounding {

  private def pow10(n: Int): Long = Math.pow(10, n).toLong

  /** @return
    *   Floor rounded (preciseAmount, fractionDigits) to the cent value of the given currency
    */
  def roundFloor(preciseAmount: Long, fractionDigits: Int, currency: Currency): Long =
    if (preciseAmount < 0L) {
      val power = pow10(fractionDigits - currency.getDefaultFractionDigits)
      val floor = preciseAmount / power
      val remainder = preciseAmount % power
      if (remainder == 0L) floor else floor - 1L
    } else
      preciseAmount / pow10(fractionDigits - currency.getDefaultFractionDigits)

  /** @return
    *   Ceiling rounded (preciseAmount, fractionDigits) to the cent value of the given currency
    */
  def roundCeiling(preciseAmount: Long, fractionDigits: Int, currency: Currency): Long =
    if (preciseAmount < 0L)
      preciseAmount / pow10(fractionDigits - currency.getDefaultFractionDigits)
    else {
      val power = pow10(fractionDigits - currency.getDefaultFractionDigits)
      val floor = preciseAmount / power
      val remainder = preciseAmount % power
      if (remainder == 0L) floor else floor + 1L
    }

  private def getFractionDigits(
      fractionWithoutLeadingZeros: Long,
      fractionDigits: Int): List[Int] = {
    @tailrec
    def loop(remainder: Long, acc: List[Int]): List[Int] = {
      val lastDigit = (remainder % 10L).toInt
      val newRemainder = remainder / 10L
      val newAcc = lastDigit :: acc
      if (newRemainder == 0L) newAcc
      else loop(newRemainder, newAcc)
    }
    val digits = loop(fractionWithoutLeadingZeros, List.empty)

    if (digits.length < fractionDigits) List.fill(fractionDigits - digits.length)(0) ::: digits
    else digits
  }

  /** @return
    *   half even rounded (preciseAmount, fractionDigits) to the cent value of the given currency
    */
  def roundHalfEven(preciseAmount: Long, fractionDigits: Int, currency: Currency): Long = {
    val centFractionDigits = fractionDigits - currency.getDefaultFractionDigits
    val power = pow10(centFractionDigits)
    val integer = preciseAmount / power
    val fraction = preciseAmount % power

    // Eg: 3 for 123.456
    val leastSignificantDigitOfInt = integer % 10L

    val fractionDigitsList = getFractionDigits(fraction, centFractionDigits)

    // Eg: 4 for 123.456
    val mostSignificantDigitOfFraction :: rest = fractionDigitsList

    if (mostSignificantDigitOfFraction == 5 && rest.forall(_ == 0))
      if (leastSignificantDigitOfInt % 2L == 0L) integer else integer + 1L
    else if (mostSignificantDigitOfFraction >= 5)
      integer + 1L
    else if (mostSignificantDigitOfFraction == -5 && rest.forall(_ == 0))
      if (leastSignificantDigitOfInt % 2L == 0L) integer else integer - 1L
    else if (mostSignificantDigitOfFraction <= -5)
      integer - 1L
    else
      integer
  }

}
