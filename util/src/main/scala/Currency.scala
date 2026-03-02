package io.sphere.util

import java.util.{Currency => JavaCurrency, Locale}
import scala.collection.JavaConverters._ // because of 2.12 compatibility

sealed trait Currency {
  def getDefaultFractionDigits: Int
  def getSymbol(locale: Locale): String
  def getCurrencyCode: String
}

object Currency {

  private object Cache {
    val EUR: JCurrency = JCurrency(JavaCurrency.getInstance("EUR"))
    val USD: JCurrency = JCurrency(JavaCurrency.getInstance("USD"))
    val GBP: JCurrency = JCurrency(JavaCurrency.getInstance("GBP"))
    val JPY: JCurrency = JCurrency(JavaCurrency.getInstance("JPY"))
  }

  def getInstance(string: String): Currency =
    string match {
      case "EUR" => Cache.EUR
      case "USD" => Cache.USD
      case "GBP" => Cache.GBP
      case "JPY" => Cache.JPY
      case _ =>
        CustomCurrency
          .fromString(string)
          .getOrElse(
            JCurrency(java.util.Currency.getInstance(string))
          )
    }

  // This is only used for tests, so no support for custom currencies
  def getInstance(locale: Locale): Currency =
    JCurrency(java.util.Currency.getInstance(locale))

  def getAvailableCurrencies: Vector[Currency] = {
    val defaultCurrencies = java.util.Currency.getAvailableCurrencies.asScala.toVector
      .map(JCurrency.apply)
    val customCurrencies = CustomCurrency.getAvailableCurrencies
    defaultCurrencies ++ customCurrencies
  }
}

case class JCurrency(currency: java.util.Currency) extends Currency {
  def getDefaultFractionDigits: Int = currency.getDefaultFractionDigits
  def getSymbol(locale: Locale): String = currency.getSymbol(locale)
  def getCurrencyCode: String = currency.getCurrencyCode
  override def toString: String = currency.toString
}

sealed abstract class CustomCurrency(
    val getDefaultFractionDigits: Int,
    parentCurrency: java.util.Currency)
    extends Currency
    with Product {
  final def getSymbol(locale: Locale): String = parentCurrency.getSymbol(locale)
  final def getCurrencyCode: String = productPrefix
  override def toString: String = productPrefix
}

object CustomCurrency {
  // Add any new currency to getAvailableCurrencies below
  case object HUF0 extends CustomCurrency(0, java.util.Currency.getInstance("HUF"))
  case object TWD0 extends CustomCurrency(0, java.util.Currency.getInstance("TWD"))
  case object TRY0 extends CustomCurrency(0, java.util.Currency.getInstance("TRY"))
  case object ILS0 extends CustomCurrency(0, java.util.Currency.getInstance("ILS"))
  case object KZT0 extends CustomCurrency(0, java.util.Currency.getInstance("KZT"))
  case object CZK0 extends CustomCurrency(0, java.util.Currency.getInstance("CZK"))

  // Wanted to use just a single immutable ArraySeq, instead of Vector & Array below, but it's not on 2.12
  val getAvailableCurrencies: Vector[CustomCurrency] =
    Vector(HUF0, TWD0, TRY0, ILS0, KZT0, CZK0)

  // Array supposed to be the fastest for the `.find` and it's the only Array that's available in all Scala versions
  private val fasterAvailableCurrencies = getAvailableCurrencies.toArray

  def fromString(string: String): Option[CustomCurrency] =
    fasterAvailableCurrencies.find(curr => curr.getCurrencyCode == string)
}
