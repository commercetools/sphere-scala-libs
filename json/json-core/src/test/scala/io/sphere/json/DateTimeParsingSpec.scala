package io.sphere.json

import org.json4s.JString
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DateTimeParsingSpec extends AnyWordSpec with Matchers {

  import FromJSON.dateTimeReader
  def jsonDateStringWith(dayOfTheMonth: Int = 23, monthOfTheYear: Int = 11, hourOfTheDay: Int = 13, minuteOfTheHour: Int = 45, secondOfTheMinute: Int = 34) : JString =
    JString(s"2035-$monthOfTheYear-${dayOfTheMonth}T$hourOfTheDay:$minuteOfTheHour:$secondOfTheMinute.543Z")

  val beValid = be(Symbol("valid"))

  "parsing a DateTime" should {
    "reject dates with invalid days" in {
      dateTimeReader.read(jsonDateStringWith(dayOfTheMonth = 59)) shouldNot beValid
    }

    "reject dates with invalid months" in {
      dateTimeReader.read(jsonDateStringWith(monthOfTheYear = 39)) shouldNot beValid
    }

    "reject dates with invalid hours" in {
      dateTimeReader.read(jsonDateStringWith(hourOfTheDay = 39)) shouldNot beValid
    }

    "reject dates with invalid minutes" in {
      dateTimeReader.read(jsonDateStringWith(minuteOfTheHour = 87)) shouldNot beValid
    }

    "reject dates with invalid seconds" in {
      dateTimeReader.read(jsonDateStringWith(secondOfTheMinute = 87)) shouldNot beValid
    }
  }
}
