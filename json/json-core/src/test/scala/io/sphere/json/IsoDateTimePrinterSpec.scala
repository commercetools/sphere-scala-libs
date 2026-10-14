package io.sphere.json

import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

/** `ToJSON.printIsoUtc` is a hand-rolled replacement for joda's ISO printer. Its only contract is
  * that it produces byte-identical output, so that is all this checks.
  */
class IsoDateTimePrinterSpec extends AnyWordSpec with Matchers {

  private def joda(dt: DateTime): String =
    ISODateTimeFormat.dateTime.print(dt.withZone(DateTimeZone.UTC))

  "printIsoUtc" must {
    "match joda on edge cases" in {
      val cases = List(
        new DateTime(0L, DateTimeZone.UTC),
        new DateTime("0001-01-01T00:00:00.000Z"),
        new DateTime("2015-12-14T12:50:25.070Z"),
        new DateTime("2015-01-01T00:00:00.000Z"),
        new DateTime("9999-12-31T23:59:59.999Z"),
        // non-UTC input must still print as UTC
        new DateTime("2015-06-01T12:00:00.000+02:00"),
        new DateTime("2015-06-01T12:00:00.000-05:30"),
        // years outside 0..9999 take the joda fallback
        new DateTime("10000-01-01T00:00:00.000Z"),
        new DateTime("-0005-01-01T00:00:00.000Z")
      )
      cases.foreach(dt => ToJSON.printIsoUtc(dt) must be(joda(dt)))
    }

    "match joda on random instants" in {
      val rnd = new Random(20260925L)
      // roughly year 1200..2750, plus the sub-second digits that trip up naive padding
      (1 to 20000).foreach { _ =>
        val dt = new DateTime(rnd.nextLong() % 25000000000000L, DateTimeZone.UTC)
        ToJSON.printIsoUtc(dt) must be(joda(dt))
      }
    }
  }
}
