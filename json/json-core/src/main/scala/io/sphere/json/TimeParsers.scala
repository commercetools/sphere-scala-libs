package io.sphere.json

import java.time
import java.time.{Instant, LocalDate}

/** Parsers for `java.time` types. Those parsers are low-level! Expect exceptions to be thrown if
  * the input is not valid.
  */
object TimeParsers {

  def parseInstant(s: String): Instant =
    Instant.from(lenientInstantParser.parse(s))

  def parseLocalDate(s: String): LocalDate =
    LocalDate.from(lenientLocalDateParser.parse(s))

  // java.time
  // this formatter is used to parse instant in an extra lenient way
  // similar to what the joda `DateTime` constructor accepts
  // the accepted grammar for joda is described here: https://www.joda.org/joda-time/apidocs/org/joda/time/format/ISODateTimeFormat.html#dateTimeParser--
  // this only supports the part where the date is specified
  private val lenientInstantParser =
    new time.format.DateTimeFormatterBuilder()
      .appendPattern("uuuu[-MM[-dd]]")
      .optionalStart()
      .appendPattern("'T'[HH[:mm[:ss]]]")
      .appendFraction(time.temporal.ChronoField.NANO_OF_SECOND, 0, 9, true)
      .optionalEnd
      .optionalStart()
      .appendOffset("+HH:MM", "Z")
      .optionalEnd()
      .optionalStart()
      .appendOffset("+HHmm", "Z")
      .optionalEnd()
      .parseDefaulting(time.temporal.ChronoField.MONTH_OF_YEAR, 1L)
      .parseDefaulting(time.temporal.ChronoField.DAY_OF_MONTH, 1L)
      .parseDefaulting(time.temporal.ChronoField.HOUR_OF_DAY, 0L)
      .parseDefaulting(time.temporal.ChronoField.MINUTE_OF_HOUR, 0L)
      .parseDefaulting(time.temporal.ChronoField.SECOND_OF_MINUTE, 0L)
      .parseDefaulting(time.temporal.ChronoField.NANO_OF_SECOND, 0L)
      .parseDefaulting(time.temporal.ChronoField.OFFSET_SECONDS, 0L)
      .toFormatter()

  private val lenientLocalDateParser =
    new time.format.DateTimeFormatterBuilder()
      .optionalStart()
      .appendLiteral('+')
      .optionalEnd()
      .appendValue(time.temporal.ChronoField.YEAR, 1, 9, java.time.format.SignStyle.NORMAL)
      .optionalStart()
      .appendLiteral('-')
      .appendValue(time.temporal.ChronoField.MONTH_OF_YEAR, 1, 2, java.time.format.SignStyle.NORMAL)
      .optionalStart()
      .appendLiteral('-')
      .appendValue(time.temporal.ChronoField.DAY_OF_MONTH, 1, 2, java.time.format.SignStyle.NORMAL)
      .optionalEnd()
      .optionalEnd()
      .parseDefaulting(time.temporal.ChronoField.MONTH_OF_YEAR, 1L)
      .parseDefaulting(time.temporal.ChronoField.DAY_OF_MONTH, 1L)
      .toFormatter()
}
