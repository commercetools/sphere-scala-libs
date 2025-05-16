package io.sphere.json

import org.json4s.JString
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.time.Instant
import cats.data.Validated.Valid

class DateTimeParsingSpec extends AnyWordSpec with Matchers {

  import FromJSON.dateTimeReader
  import FromJSON.javaInstantReader
  def jsonDateStringWith(
      year: String = "2035",
      dayOfTheMonth: String = "23",
      monthOfTheYear: String = "11",
      hourOfTheDay: String = "13",
      minuteOfTheHour: String = "45",
      secondOfTheMinute: String = "34",
      millis: String = "543"): JString =
    JString(
      s"$year-$monthOfTheYear-${dayOfTheMonth}T$hourOfTheDay:$minuteOfTheHour:$secondOfTheMinute.${millis}Z")

  val beValid = be(Symbol("valid"))
  val outOfIntRange = "999999999999999"

  "parsing a DateTime" should {

    "reject strings with invalid year" in {
      dateTimeReader.read(jsonDateStringWith(year = "999999999")) shouldNot beValid
    }

    "reject strings with years that are out of range for integers" in {
      dateTimeReader.read(jsonDateStringWith(year = outOfIntRange)) shouldNot beValid
    }

    "reject strings that are out of range for other fields" in {
      dateTimeReader.read(
        jsonDateStringWith(
          monthOfTheYear = outOfIntRange,
          dayOfTheMonth = outOfIntRange,
          hourOfTheDay = outOfIntRange,
          minuteOfTheHour = outOfIntRange,
          secondOfTheMinute = outOfIntRange,
          millis = outOfIntRange
        )) shouldNot beValid
    }

    "reject strings with invalid days" in {
      dateTimeReader.read(jsonDateStringWith(dayOfTheMonth = "59")) shouldNot beValid
    }

    "reject strings with invalid months" in {
      dateTimeReader.read(jsonDateStringWith(monthOfTheYear = "39")) shouldNot beValid
    }

    "reject strings with invalid hours" in {
      dateTimeReader.read(jsonDateStringWith(hourOfTheDay = "39")) shouldNot beValid
    }

    "reject strings with invalid minutes" in {
      dateTimeReader.read(jsonDateStringWith(minuteOfTheHour = "87")) shouldNot beValid
    }

    "reject strings with invalid seconds" in {
      dateTimeReader.read(jsonDateStringWith(secondOfTheMinute = "87")) shouldNot beValid
    }
  }

  "parsing an Instant" should {

    "reject strings with invalid year" in {
      javaInstantReader.read(jsonDateStringWith(year = "999999999")) shouldNot beValid
    }

    "reject strings with years that are out of range for integers" in {
      javaInstantReader.read(jsonDateStringWith(year = outOfIntRange)) shouldNot beValid
    }

    "reject strings that are out of range for other fields" in {
      javaInstantReader.read(
        jsonDateStringWith(
          monthOfTheYear = outOfIntRange,
          dayOfTheMonth = outOfIntRange,
          hourOfTheDay = outOfIntRange,
          minuteOfTheHour = outOfIntRange,
          secondOfTheMinute = outOfIntRange,
          millis = outOfIntRange
        )) shouldNot beValid
    }

    "reject strings with invalid days" in {
      javaInstantReader.read(jsonDateStringWith(dayOfTheMonth = "59")) shouldNot beValid
    }

    "reject strings with invalid months" in {
      javaInstantReader.read(jsonDateStringWith(monthOfTheYear = "39")) shouldNot beValid
    }

    "reject strings with invalid hours" in {
      javaInstantReader.read(jsonDateStringWith(hourOfTheDay = "39")) shouldNot beValid
    }

    "reject strings with invalid minutes" in {
      javaInstantReader.read(jsonDateStringWith(minuteOfTheHour = "87")) shouldNot beValid
    }

    "reject strings with invalid seconds" in {
      javaInstantReader.read(jsonDateStringWith(secondOfTheMinute = "87")) shouldNot beValid
    }
  }

  // ported from https://github.com/JodaOrg/joda-time/blob/4a1402a47cab4636bf4c73d42a62bfa80c1535ca/src/test/java/org/joda/time/convert/TestStringConverter.java#L114-L156
  // ensures that we accept similar patterns as joda when parsing instants
  "parsing a Java instant" should {
    "accept a full instant with milliseconds and offset" in {
      javaInstantReader.read(JString("2004-06-09T12:24:48.501+0800")) shouldBe Valid(
        Instant.parse("2004-06-09T04:24:48.501Z"))
    }

    "accept a year with offset" in {
      javaInstantReader.read(JString("2004T+0800")) shouldBe Valid(
        Instant.parse("2004-01-01T00:00:00+08:00"))
    }

    "accept a year month with offset" in {
      javaInstantReader.read(JString("2004-06T+0800")) shouldBe Valid(
        Instant.parse("2004-06-01T00:00:00+08:00"))
    }

    "accept a year month day without offset" in {
      javaInstantReader.read(JString("2004-06-09")) shouldBe Valid(
        Instant.parse("2004-06-09T00:00:00+00:00"))
    }

    "accept a year month day with offset" in {
      javaInstantReader.read(JString("2004-06-09T+0800")) shouldBe Valid(
        Instant.parse("2004-06-09T00:00:00+08:00"))
    }

    "accept a year month day with hour and offset" in {
      javaInstantReader.read(JString("2004-06-09T12+0800")) shouldBe Valid(
        Instant.parse("2004-06-09T04:00:00Z"))
    }

    "accept a year month day with hour, minute, and offset" in {
      javaInstantReader.read(JString("2004-06-09T12:24+0800")) shouldBe Valid(
        Instant.parse("2004-06-09T04:24:00Z"))
    }

    "accept a year month day with hour, minute, second, and offset" in {
      javaInstantReader.read(JString("2004-06-09T12:24:48+0800")) shouldBe Valid(
        Instant.parse("2004-06-09T04:24:48Z"))
    }

    "accept a year month day with hour, fraction, and offset" in {
      javaInstantReader.read(JString("2004-06-09T12.5+0800")) shouldBe Valid(
        Instant.parse("2004-06-09T04:00:00.5Z"))
    }

    "accept a year month day with hour, minute, fraction, and offset" in {
      javaInstantReader.read(JString("2004-06-09T12:24.5+0800")) shouldBe Valid(
        Instant.parse("2004-06-09T04:24:00.5Z"))
    }

    "accept a year month day with hour, minute, second, fraction, and offset" in {
      javaInstantReader.read(JString("2004-06-09T12:24:48.5+0800")) shouldBe Valid(
        Instant.parse("2004-06-09T04:24:48.5Z"))
    }

    "accept a year month day with hour, minute, second, fraction, but no offset" in {
      javaInstantReader.read(JString("2004-06-09T12:24:48.501")) shouldBe Valid(
        Instant.parse("2004-06-09T12:24:48.501Z"))
    }

    "accept a year month day with hour, minute, second, and an offset containing a colon" in {
      javaInstantReader.read(JString("2004-06-09T12:24:48+08:00")) shouldBe Valid(
        Instant.parse("2004-06-09T04:24:48Z"))
    }
    "accept a year month day with hour, minute, second, and an offset of zero containing a colon" in {
      javaInstantReader.read(JString("2004-06-09T12:24:48+00:00")) shouldBe Valid(
        Instant.parse("2004-06-09T12:24:48Z"))
    }
    "accept a year month day with hour, minute, second, and a negative colon offset" in {
      javaInstantReader.read(JString("2004-06-09T12:24:48-08:00")) shouldBe Valid(
        Instant.parse("2004-06-09T20:24:48Z"))
    }
    "accept a year month day with hour, minute, second, and an offset of hours only" in {
      javaInstantReader.read(JString("2004-06-09T12:24:48+00")) shouldBe Valid(
        Instant.parse("2004-06-09T12:24:48Z"))
    }
  }

}
