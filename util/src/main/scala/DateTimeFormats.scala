package io.sphere.util

import scala.util.Try

import java.time.format._
import java.time.temporal.ChronoField
import java.time.LocalDate
import java.time.LocalTime
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.OffsetDateTime
import java.time.Instant

object DateTimeFormats {

  /** Canonical format for writing dates.
    *
    * Very close to the default ISO_LOCAL_DATE formatter with the difference that the sign in front
    * of the year is only used to mark negative years (i.e. no + for years with more than 4 digits)
    */
  val localDateFormatter: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendValue(ChronoField.YEAR, 4, 9, SignStyle.NORMAL)
    .appendLiteral('-')
    .appendValue(ChronoField.MONTH_OF_YEAR, 2)
    .appendLiteral('-')
    .appendValue(ChronoField.DAY_OF_MONTH, 2)
    .toFormatter();

  /** Canonical format for writing times.
    */
  val localTimeFormatter: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendValue(ChronoField.HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
    .appendLiteral(':')
    .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
    .appendLiteral('.')
    .appendValue(ChronoField.MILLI_OF_SECOND, 3)
    .toFormatter()

  /** Canonical format for writing date-times in UTC
    */
  val localDateTimeFormatter = new DateTimeFormatterBuilder()
    .append(localDateFormatter)
    .appendLiteral('T')
    .append(localTimeFormatter)
    .appendLiteral('Z')
    .toFormatter

  def format(date: LocalDate): String = localDateFormatter.format(date)
  def format(time: LocalTime): String = localTimeFormatter.format(time)
  def format(dateTime: LocalDateTime): String =
    localDateTimeFormatter.format(dateTime.atOffset(ZoneOffset.UTC).toLocalDateTime())
  def format(dateTime: ZonedDateTime): String =
    localDateTimeFormatter.format(dateTime.withZoneSameInstant(ZoneOffset.UTC).toLocalDateTime())
  def format(dateTime: OffsetDateTime): String =
    localDateTimeFormatter.format(dateTime.withOffsetSameInstant(ZoneOffset.UTC).toLocalDateTime())
  def format(instant: Instant): String =
    localDateTimeFormatter.format(instant.atZone(ZoneOffset.UTC).toLocalDateTime())

  /** This is adapted from
    * https://github.com/commercetools/sphere-scala-libs/blob/dcb270c4be706b8c8b84d43821e48d1982fd6c37/json/json-core/src/main/scala/io/sphere/json/FromJSON.scala#L408
    *
    * The original [[LocalDate]] parser was created to retain the leniency of Joda after migrating
    * from Joda to [[java.time]] in Sphere. java.time operates more strictly, so this parser ensures
    * a very high level in flexibility
    */
  val lenientLocalDateParser: DateTimeFormatter = new DateTimeFormatterBuilder()
    .optionalStart()
    .appendLiteral('+')
    .optionalEnd()
    .appendValue(ChronoField.YEAR, 1, 9, SignStyle.NORMAL)
    .optionalStart()
    .appendLiteral('-')
    .appendValue(ChronoField.MONTH_OF_YEAR, 1, 2, SignStyle.NORMAL)
    .optionalStart()
    .appendLiteral('-')
    .appendValue(ChronoField.DAY_OF_MONTH, 1, 2, SignStyle.NORMAL)
    .optionalEnd()
    .optionalEnd()
    .parseDefaulting(ChronoField.MONTH_OF_YEAR, 1L)
    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1L)
    .toFormatter()

  // Simplified version of jodatime's `ISODateTimeFormat.localTimeParser`
  val lenientLocalTimeParser: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendValue(ChronoField.HOUR_OF_DAY, 2)
    .optionalStart()
    .appendLiteral(':')
    .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
    .optionalStart()
    .appendLiteral(':')
    .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
    .optionalEnd()
    .optionalEnd()
    .optionalStart()
    .appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
    .optionalEnd()
    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
    .parseDefaulting(ChronoField.NANO_OF_SECOND, 0)
    .toFormatter()

  /** Strict version o jodatime's `ISODateTimeFormat.dateTimeParser`
    *
    * Parse a ZonedDateTime defaulting to UTC using the lenient date and time parsers
    */
  val lenientDateTimeParser: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .append(lenientLocalDateParser)
      .optionalStart()
      .appendLiteral('T')
      .optionalStart()
      .append(lenientLocalTimeParser)
      .optionalEnd()
      .optionalStart()
      .optionalStart()
      .appendOffsetId()
      .optionalEnd()
      .optionalStart()
      .appendOffset("+HH:MM", "Z")
      .optionalEnd()
      .optionalStart()
      .appendOffset("+HHmm", "Z")
      .optionalEnd()
      .optionalEnd()
      .optionalEnd()
      .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
      .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
      .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
      .parseDefaulting(ChronoField.NANO_OF_SECOND, 0)
      .parseDefaulting(ChronoField.OFFSET_SECONDS, 0)
      .toFormatter

  def parseLocalTime(time: String): Try[LocalTime] = Try(
    LocalTime.parse(time, lenientLocalTimeParser))
  def parseLocalDate(date: String): Try[LocalDate] = Try(
    LocalDate.parse(date, lenientLocalDateParser))
  def parseLocalDateTime(dateTime: String): Try[LocalDateTime] =
    Try(
      OffsetDateTime
        .parse(dateTime, lenientDateTimeParser)
        .withOffsetSameInstant(ZoneOffset.UTC)
        .toLocalDateTime
    )
  def parseOffsetDateTime(dateTime: String): Try[OffsetDateTime] =
    Try(
      OffsetDateTime.parse(dateTime, lenientDateTimeParser)
    )
  def parseInstant(dateTime: String): Try[Instant] =
    Try(
      OffsetDateTime.parse(dateTime, lenientDateTimeParser).toInstant()
    )
}
