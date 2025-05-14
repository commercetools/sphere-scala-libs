package io.sphere.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.language.postfixOps
import org.scalatest.prop.Tables
import java.time.LocalDateTime

import org.scalatest.prop.TableDrivenPropertyChecks._
import java.time.Instant
import java.time.ZoneOffset
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import java.time.LocalDate

class DateTimeFormatsSpec extends AnyFunSpec with Matchers {

  describe("DateTimeFormats") {
    it("should parse date-times supported by sphere") {

      val dates = Tables.Table(
        ("input", "expected", "serialized"),
        (
          "111112018-10-12T14:30:15.123456789Z",
          LocalDateTime.of(111112018, 10, 12, 14, 30, 15, 123456789),
          "111112018-10-12T14:30:15.123Z"
        ),
        (
          "11112018-10-12T14:30:15.12345678Z",
          LocalDateTime.of(11112018, 10, 12, 14, 30, 15, 123456780),
          "11112018-10-12T14:30:15.123Z"
        ),
        (
          "1112018-10-12T14:30:15.1234567Z",
          LocalDateTime.of(1112018, 10, 12, 14, 30, 15, 123456700),
          "1112018-10-12T14:30:15.123Z"
        ),
        (
          "112018-10-12T14:30:15.123456Z",
          LocalDateTime.of(112018, 10, 12, 14, 30, 15, 123456000),
          "112018-10-12T14:30:15.123Z"
        ),
        (
          "12018-10-12T14:30:15.12345Z",
          LocalDateTime.of(12018, 10, 12, 14, 30, 15, 123450000),
          "12018-10-12T14:30:15.123Z"
        ),
        (
          "2018-10-12T14:30:15.1234Z",
          LocalDateTime.of(2018, 10, 12, 14, 30, 15, 123400000),
          "2018-10-12T14:30:15.123Z"
        ),
        (
          "2018-10-12T14:30:15.123Z",
          LocalDateTime.of(2018, 10, 12, 14, 30, 15, 123000000),
          "2018-10-12T14:30:15.123Z"
        ),
        (
          "2018-10-12T14:30:15.123Z",
          LocalDateTime.of(2018, 10, 12, 14, 30, 15, 123000000),
          "2018-10-12T14:30:15.123Z"
        ),
        (
          "2018-10-12T14:30:15.123+02",
          LocalDateTime.of(2018, 10, 12, 12, 30, 15, 123000000),
          "2018-10-12T12:30:15.123Z"
        ),
        (
          "2018-10-12T14:30:15.123+02:15",
          LocalDateTime.of(2018, 10, 12, 12, 15, 15, 123000000),
          "2018-10-12T12:15:15.123Z"
        ),
        (
          "2018-10-12T14:30:15.123+02:15:15",
          LocalDateTime.of(2018, 10, 12, 12, 15, 0, 123000000),
          "2018-10-12T12:15:00.123Z"
        ),
        (
          "2018-10-12T14:30:15.123-02",
          LocalDateTime.of(2018, 10, 12, 16, 30, 15, 123000000),
          "2018-10-12T16:30:15.123Z"
        ),
        (
          "2018-10-12T14:30:15.123-02:15",
          LocalDateTime.of(2018, 10, 12, 16, 45, 15, 123000000),
          "2018-10-12T16:45:15.123Z"
        ),
        (
          "2018-10-12T14:30:15.123-02:15:15",
          LocalDateTime.of(2018, 10, 12, 16, 45, 30, 123000000),
          "2018-10-12T16:45:30.123Z"
        ),
        (
          "2018-10-12T14:30:15",
          LocalDateTime.of(2018, 10, 12, 14, 30, 15, 0),
          "2018-10-12T14:30:15.000Z"
        ),
        (
          "2018-10-12T14:30",
          LocalDateTime.of(2018, 10, 12, 14, 30, 0, 0),
          "2018-10-12T14:30:00.000Z"
        ),
        (
          "2018-10-12T14",
          LocalDateTime.of(2018, 10, 12, 14, 0, 0, 0),
          "2018-10-12T14:00:00.000Z"
        ),
        (
          "2018-10T14",
          LocalDateTime.of(2018, 10, 1, 14, 0, 0, 0),
          "2018-10-01T14:00:00.000Z"
        ),
        (
          "2018T14",
          LocalDateTime.of(2018, 1, 1, 14, 0, 0, 0),
          "2018-01-01T14:00:00.000Z"
        )
      )
      forAll(dates) { (inputString, expected, serializedString) =>
        val parsedDateTime = DateTimeFormats.parseLocalDateTime(inputString)

        withClue("parsed date-time does not match expected") {
          parsedDateTime must be(Success(expected))
        }

        withClue("serialized does not match expected") {
          DateTimeFormats.format(parsedDateTime.get) must be(serializedString)
        }
      }
    }

    it("should parse local dates with five digits") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("12345-12-31")
      )(year = 12345, month = 12, day = 31)
    }

    it("should parse local dates with five digits prefixed by a plus sign") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("+12345-12-31")
      )(year = 12345, month = 12, day = 31)
    }

    it("should parse local dates with zero padded days") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("2024-12-01")
      )(year = 2024, month = 12, day = 1)
    }
    it("should parse local dates with non-zero padded days") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("2024-12-1")
      )(year = 2024, month = 12, day = 1)
    }
    it("should parse local dates with zero padded months") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("2024-01-31")
      )(year = 2024, month = 1, day = 31)
    }
    it("should parse local dates with non-zero padded months") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("2024-1-31")
      )(year = 2024, month = 1, day = 31)
    }
    it("should parse local dates with missing day as first day of the month") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("2024-1")
      )(year = 2024, month = 1, day = 1)
    }
    it("should parse local dates with missing day and month as first day of the first month") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("2024")
      )(year = 2024, month = 1, day = 1)
    }
    it("should accept two digit years") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("50-01-31")
      )(year = 50, month = 1, day = 31)
    }
    it("should accept negative years") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("-50-01-31")
      )(year = -50, month = 1, day = 31)
    }
    it("should accept negative four digit years") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("-2024-01-31")
      )(year = -2024, month = 1, day = 31)
    }
    it("should accept a year with a leading zero") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("02024-01-31")
      )(year = 2024, month = 1, day = 31)
    }
    it("should accept a nine digit year") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("123456789-01-31")
      )(year = 123456789, month = 1, day = 31)
    }
    it("should accept year zero") {
      assertDecodeResult(
        DateTimeFormats.parseLocalDate("0-01-31")
      )(year = 0, month = 1, day = 31)
    }
    it("should not accept a zero day") {
      assert(DateTimeFormats.parseLocalDate("2024-01-00").isFailure)
    }
    it("should not accept a zero month") {
      assert(DateTimeFormats.parseLocalDate("2024-00-01").isFailure)
    }
    it("should not accept days greater than 31") {
      assert(DateTimeFormats.parseLocalDate("2024-12-32").isFailure)
    }
    it("should not accept months greater than 12") {
      assert(DateTimeFormats.parseLocalDate("2024-13-31").isFailure)
    }
  }

  private def assertDecodeResult(
      decodeResult: Try[LocalDate]
  )(year: Int, month: Int, day: Int) = decodeResult match {
    case Success(localDate) =>
      localDate.getYear() mustBe year
      localDate.getMonthValue() mustBe month
      localDate.getDayOfMonth() mustBe day
    case Failure(e) => fail(s"unable to parse date string failed with $e")
  }
}
