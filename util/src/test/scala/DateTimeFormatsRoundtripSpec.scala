package io.sphere.util

import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime => JodaDateTime, DateTimeZone => JodaDateTimeZone, LocalDate => JodaLocalDate, LocalTime => JodaLocalTime}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZoneOffset, ZonedDateTime}

class DateTimeFormatsRoundtripSpec extends Properties("DateTimeFormats roundtrip") {
  val epochMillis = Gen.choose(
    ZonedDateTime
      .of(LocalDateTime.of(-10000, 1, 1, 0, 0, 0), ZoneOffset.UTC)
      .toInstant()
      .toEpochMilli(),
    ZonedDateTime
      .of(LocalDateTime.of(+10000, 12, 31, 23, 59, 59), ZoneOffset.UTC)
      .toInstant()
      .toEpochMilli()
  )

  val zoneOffset = Gen.choose(-10, +10).map(ZoneOffset.ofHours)

  implicit def arbitraryDateTime: Arbitrary[OffsetDateTime] =
    Arbitrary(
      epochMillis.map(ts => OffsetDateTime.ofInstant(Instant.ofEpochMilli(ts), ZoneOffset.UTC)))

  implicit val arbitraryInstant: Arbitrary[Instant] =
    Arbitrary(epochMillis.map(Instant.ofEpochMilli))

  implicit val arbitratyLocalTime: Arbitrary[LocalTime] = Arbitrary(
    Gen.choose(0, 3600 * 24 - 1).map(LocalTime.ofSecondOfDay(_))
  )

  implicit val arbitratyLocalDate: Arbitrary[LocalDate] = Arbitrary(
    epochMillis
      .map(Instant.ofEpochMilli(_).atZone(ZoneOffset.UTC))
      .map(_.toLocalDate())
  )

  def jodaFormat(dateTime: JodaDateTime): String =
    ISODateTimeFormat.dateTime.print(dateTime.withZone(JodaDateTimeZone.UTC))

  def jodaFormat(time: JodaLocalTime): String =
    ISODateTimeFormat.time.print(time)

  def jodaFormat(date: JodaLocalDate): String =
    ISODateTimeFormat.date.print(date)

  property("compatibility between serialized OffsetDateTime and joda.time.DateTime") = Prop.forAll {
    (instant: Instant) =>
      val javaDateTime = instant.atOffset(ZoneOffset.UTC)
      val jodaDateTime = new JodaDateTime(instant.toEpochMilli(), JodaDateTimeZone.UTC)

      val serializedJava = DateTimeFormats.format(javaDateTime)
      val serializedJoda = jodaFormat(jodaDateTime)
      val res = serializedJava == serializedJoda
      if (!res) {
        println("------")
        println(serializedJava)
        println(serializedJoda)
      }
      res
  }

  property("compatibility between serialized java.time.LocalTime and org.joda.time.LocalTime") =
    Prop.forAll { (javaTime: LocalTime) =>
      val jodaTime = JodaLocalTime.fromMillisOfDay(javaTime.toNanoOfDay() / 1000000)

      val serializedJava = DateTimeFormats.format(javaTime)
      val serializedJoda = jodaFormat(jodaTime)
      serializedJava == serializedJoda
    }

  property("compatibility between serialized java.time.LocalDate and org.joda.time.LocalDate") =
    Prop.forAll { (javaDate: LocalDate) =>
      val jodaDate =
        new JodaLocalDate(javaDate.getYear(), javaDate.getMonthValue(), javaDate.getDayOfMonth())

      val serializedJava = DateTimeFormats.format(javaDate)
      val serializedJoda = jodaFormat(jodaDate)
      serializedJava == serializedJoda
    }

  property("roundtrip from java.time.OffsetDateTime") = Prop.forAll {
    (instant: Instant, offset: ZoneOffset) =>
      val source = instant.atOffset(offset)
      val serialized = DateTimeFormats.format(source)
      val deserialized = DateTimeFormats.parseOffsetDateTime(serialized)

      deserialized.fold(_ => false, _.toInstant() == source.toInstant())
  }

  property("roundtrip from java.time.Instant") = Prop.forAll { (instant: Instant) =>
    val serialized = DateTimeFormats.format(instant)
    val deserialized = DateTimeFormats.parseInstant(serialized)

    deserialized.fold(_ => false, _ == instant)
  }

  property("roundtrip from java.time.LocalDateTime") = Prop.forAll { (instant: Instant) =>
    val source = instant.atOffset(ZoneOffset.UTC).toLocalDateTime()
    val serialized = DateTimeFormats.format(source)
    val deserialized = DateTimeFormats.parseLocalDateTime(serialized)

    deserialized.fold(_ => false, _ == source)
  }
}
