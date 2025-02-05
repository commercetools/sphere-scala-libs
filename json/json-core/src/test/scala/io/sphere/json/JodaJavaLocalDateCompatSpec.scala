package io.sphere.json

import org.json4s.JString
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.time.Instant
import cats.data.Validated.Valid

class JodaJavaLocalDateCompatSpec extends AnyWordSpec with Matchers {

  val jodaReader = FromJSON.dateReader
  val javaReader = FromJSON.javaLocalDateReader
  def jsonDateStringWith(
      year: String = "2035",
      dayOfTheMonth: String = "23",
      monthOfTheYear: String = "11"): JString = JString(s"$year-$monthOfTheYear-${dayOfTheMonth}")

  private def test(value: JString) =
    (jodaReader.read(value), javaReader.read(value)) match {
      case (Valid(jodaDate), Valid(javaDate)) =>
        jodaDate.getYear shouldBe javaDate.getYear
        jodaDate.getMonthOfYear shouldBe javaDate.getMonthValue
        jodaDate.getDayOfMonth shouldBe javaDate.getDayOfMonth
      case (jodaDate, javaDate) =>
        fail(s"invalid date. joda: $jodaDate, java: $javaDate")
    }

  "parsing a LocalDate" should {
    "accept two digit years" in
      test(jsonDateStringWith(year = "50"))
    "accept year zero" in
      test(JString("0-10-31"))
    "accept no day set" in
      test(JString("2024-09"))
    "accept up to nine digit years" in
      (1 to 9).foreach { l =>
        val year = List.fill(l)("1").mkString("")
        test(jsonDateStringWith(year = year))
      }
    "accept a year with leading zero" in
      test(jsonDateStringWith(year = "02020"))
    "accept a year with leading plus sign" in
      test(jsonDateStringWith(year = "+02020"))
  }
}
