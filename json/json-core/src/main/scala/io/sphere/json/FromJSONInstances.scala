package io.sphere.json

import scala.util.control.NonFatal
import scala.collection.mutable.ListBuffer
import java.util.{Currency, Locale, UUID}
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.apply._
import cats.syntax.traverse._
import io.sphere.json.field
import io.sphere.util.{BaseMoney, DateTimeFormats, HighPrecisionMoney, LangTag, Logging, Money}
import org.json4s.JsonAST._

import java.time
import java.util.{Currency, Locale, UUID}
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.YearMonth
import org.joda.time.LocalTime
import org.joda.time.LocalDate
import org.joda.time.format.ISODateTimeFormat

object FromJSONInstances {
  val validNone = Valid(None)
  val validNil = Valid(Nil)
  val validEmptyAnyVector: Valid[Vector[Any]] = Valid(Vector.empty)
  def validList[A]: Valid[List[A]] = validNil
  def validEmptyVector[A]: Valid[Vector[A]] =
    validEmptyAnyVector.asInstanceOf[Valid[Vector[A]]]
}

trait FromJSONInstances extends Logging {}
