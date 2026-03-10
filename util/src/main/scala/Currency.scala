package io.sphere.util

import java.util.{Currency => JavaCurrency, Locale}
import scala.collection.JavaConverters._ // because of 2.12 compatibility

sealed trait Currency {
  def getDefaultFractionDigits: Int
  def getSymbol(locale: Locale): String
  def getCurrencyCode: String
}

object Currency {

  object Cache {
    val EUR: JCurrency = JCurrency(JavaCurrency.getInstance("EUR"))
    val USD: JCurrency = JCurrency(JavaCurrency.getInstance("USD"))
    val GBP: JCurrency = JCurrency(JavaCurrency.getInstance("GBP"))
    val DKK: JCurrency = JCurrency(JavaCurrency.getInstance("DKK"))
    val CHF: JCurrency = JCurrency(JavaCurrency.getInstance("CHF"))
    val SEK: JCurrency = JCurrency(JavaCurrency.getInstance("SEK"))
    val AUD: JCurrency = JCurrency(JavaCurrency.getInstance("AUD"))
    val NOK: JCurrency = JCurrency(JavaCurrency.getInstance("NOK"))
    val PLN: JCurrency = JCurrency(JavaCurrency.getInstance("PLN"))
    val CAD: JCurrency = JCurrency(JavaCurrency.getInstance("CAD"))
  }

  def getInstance(string: String): Currency =
    // all ISO currencies have 3 characters
    if (string.length == 4) CustomCurrency.fromString(string)
    else
      string match {
        case "EUR" => Cache.EUR
        case "USD" => Cache.USD
        case "GBP" => Cache.GBP
        case "DKK" => Cache.DKK
        case "CHF" => Cache.CHF
        case "SEK" => Cache.SEK
        case "AUD" => Cache.AUD
        case "NOK" => Cache.NOK
        case "PLN" => Cache.PLN
        case "CAD" => Cache.CAD
        case _ => JCurrency(java.util.Currency.getInstance(string))
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
  // Add any new currency all below methods and lists
  case object HUF0 extends CustomCurrency(0, java.util.Currency.getInstance("HUF"))
  case object TWD0 extends CustomCurrency(0, java.util.Currency.getInstance("TWD"))
  case object TRY0 extends CustomCurrency(0, java.util.Currency.getInstance("TRY"))
  case object ILS0 extends CustomCurrency(0, java.util.Currency.getInstance("ILS"))
  case object KZT0 extends CustomCurrency(0, java.util.Currency.getInstance("KZT"))
  case object CZK0 extends CustomCurrency(0, java.util.Currency.getInstance("CZK"))

  val getAvailableCurrencies: Vector[CustomCurrency] =
    Vector(HUF0, TWD0, TRY0, ILS0, KZT0, CZK0)

  def fromString(input: String): CustomCurrency = input match {
    case "HUF0" => HUF0
    case "TWD0" => TWD0
    case "TRY0" => TRY0
    case "ILS0" => ILS0
    case "KZT0" => KZT0
    case "CZK0" => CZK0
    case _ =>
      throw new IllegalArgumentException(s"Currency '$input' not valid as a custom currency code.")
  }
}
