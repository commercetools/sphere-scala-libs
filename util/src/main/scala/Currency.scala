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
    val EUR = JCurrency(JavaCurrency.getInstance("EUR"))
    val USD = JCurrency(JavaCurrency.getInstance("USD"))
    val GBP = JCurrency(JavaCurrency.getInstance("GBP"))
    val JPY = JCurrency(JavaCurrency.getInstance("JPY"))
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
    val customCurrencies = CustomCurrency.getAvailableCurrencies.map(CustomCurrency.apply)
    defaultCurrencies ++ customCurrencies
  }
}

case class JCurrency(currency: java.util.Currency) extends Currency {
  def getDefaultFractionDigits: Int = currency.getDefaultFractionDigits
  def getSymbol(locale: Locale): String = currency.getSymbol(locale)
  def getCurrencyCode: String = currency.getCurrencyCode
  override def toString: String = currency.toString
}

sealed abstract class AbstractCustomCurrency(
    val getDefaultFractionDigits: Int,
    parentCurrency: java.util.Currency)
    extends Product {
  final def getSymbol(locale: Locale): String = parentCurrency.getSymbol(locale)
  final def getCurrencyCode: String = parentCurrency.getCurrencyCode
  override def toString: String = parentCurrency.toString
}

case class CustomCurrency(currency: AbstractCustomCurrency) extends Currency {
  def getDefaultFractionDigits: Int = currency.getDefaultFractionDigits
  def getSymbol(locale: Locale): String = currency.getSymbol(locale)
  def getCurrencyCode: String = currency.productPrefix
}

object CustomCurrency {

  def getAvailableCurrencies: Vector[AbstractCustomCurrency] =
    Vector(HUF0, TWD0, TRY0, ILS0, KZT0, CZK0)

  def fromString(string: String): Option[CustomCurrency] =
    getAvailableCurrencies.find(_.productPrefix == string).map(curr => CustomCurrency(curr))
}

// Add any new currency to getAvailableCurrencies above
case object HUF0 extends AbstractCustomCurrency(0, java.util.Currency.getInstance("HUF"))
case object TWD0 extends AbstractCustomCurrency(0, java.util.Currency.getInstance("TWD"))
case object TRY0 extends AbstractCustomCurrency(0, java.util.Currency.getInstance("TRY"))
case object ILS0 extends AbstractCustomCurrency(0, java.util.Currency.getInstance("ILS"))
case object KZT0 extends AbstractCustomCurrency(0, java.util.Currency.getInstance("KZT"))
case object CZK0 extends AbstractCustomCurrency(0, java.util.Currency.getInstance("CZK"))
