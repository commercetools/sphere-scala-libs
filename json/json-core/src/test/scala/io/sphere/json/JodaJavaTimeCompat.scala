package io.sphere.json

import org.joda.time.{DateTime, DateTimeZone, LocalTime}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

import java.time.{Instant => JInstant, LocalTime => JLocalTime}

class JodaJavaTimeCompat extends Properties("Joda - java.time compat") {
  val epochMillis = Gen.choose(-188395027761000L, 64092207599999L)

  implicit def arbitraryDateTime: Arbitrary[DateTime] =
    Arbitrary(epochMillis.map(new DateTime(_, DateTimeZone.UTC)))

  // generate dates between years -4000 and +4000
  implicit val javaInstant: Arbitrary[JInstant] =
    Arbitrary(epochMillis.map(JInstant.ofEpochMilli(_)))

  implicit val javaLocalTime: Arbitrary[JLocalTime] = Arbitrary(
    Gen.choose(0, 3600 * 24 - 1).map(JLocalTime.ofSecondOfDay(_)))

  property("compatibility between serialized Instant and DateTime") = Prop.forAll {
    (instant: JInstant) =>
      val dateTime = new DateTime(instant.toEpochMilli(), DateTimeZone.UTC)
      val serializedInstant = ToJSON[JInstant].write(instant)
      val serializedDateTime = ToJSON[DateTime].write(dateTime)
      serializedInstant == serializedDateTime
  }

  property("compatibility between serialized java.time.LocalTime and org.joda.time.LocalTime") =
    Prop.forAll { (javaTime: JLocalTime) =>
      val jodaTime = LocalTime.fromMillisOfDay(javaTime.toNanoOfDay() / 1000000)
      val serializedJavaTime = ToJSON[JLocalTime].write(javaTime)
      val serializedJodaTime = ToJSON[LocalTime].write(jodaTime)
      serializedJavaTime == serializedJodaTime
    }

  property("roundtrip from java.time.Instant") = Prop.forAll { (instant: JInstant) =>
    FromJSON[DateTime]
      .read(ToJSON[JInstant].write(instant))
      .andThen { dateTime =>
        FromJSON[JInstant].read(ToJSON[DateTime].write(dateTime))
      }
      .fold(_ => false, _ == instant)
  }

  property("roundtrip from org.joda.time.DateTime") = Prop.forAll { (dateTime: DateTime) =>
    FromJSON[JInstant]
      .read(ToJSON[DateTime].write(dateTime))
      .andThen { instant =>
        FromJSON[DateTime].read(ToJSON[JInstant].write(instant))
      }
      .fold(_ => false, _ == dateTime)
  }

}
