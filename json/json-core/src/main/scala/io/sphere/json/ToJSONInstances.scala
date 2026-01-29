package io.sphere.json

import cats.data.NonEmptyList

import java.util.{Currency, Locale, UUID}
import java.time
import io.sphere.util.{BaseMoney, DateTimeFormats, HighPrecisionMoney, Money}
import org.json4s.JsonAST._
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.LocalTime
import org.joda.time.LocalDate
import org.joda.time.YearMonth
import org.joda.time.format.ISODateTimeFormat

object ToJSONInstances {
  val emptyJArray = JArray(Nil)
  val emptyJObject = JObject(Nil)
}

trait ToJSONInstances {}
