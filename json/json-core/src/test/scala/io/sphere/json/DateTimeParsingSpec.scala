package io.sphere.json

import org.json4s.JString
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DateTimeParsingSpec extends AnyWordSpec with Matchers {

  import FromJSON.dateTimeReader
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
}
