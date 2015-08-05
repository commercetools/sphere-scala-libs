package io.sphere.json

import scala.language.higherKinds
import scalaz._
import Scalaz._

import io.sphere.util.Money

import java.util.{ Locale, Currency, UUID }

import org.joda.time._
import org.scalacheck._

object JSONProperties extends Properties("JSON") {
  private def check[A: FromJSON: ToJSON: Equal](a: A): Boolean = {
    val json = s"""[${toJSON(a)}]"""
    val result = fromJSON[Seq[A]](json).toOption.map(_.apply(0))
    result === Some(a)
  }

  implicit def arbitraryVector[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(_.toVector))

  implicit def arbitraryNEL[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary(for {
      a <- Arbitrary.arbitrary[A]
      l <- Arbitrary.arbitrary[List[A]]
    } yield l.toNel | NonEmptyList(a))

  implicit def arbitraryCurrency: Arbitrary[Currency] =
    Arbitrary(Gen.oneOf(
      Currency.getInstance("EUR"),
      Currency.getInstance("USD"),
      Currency.getInstance("JPY")))

  implicit def arbitraryLocale: Arbitrary[Locale] = {
    // Filter because OS X thinks that 'C' and 'POSIX' are valid locales...
    val locales = Locale.getAvailableLocales().filter(_.toLanguageTag() != "und")
    Arbitrary(for {
      i <- Gen.choose(0, locales.length-1)
    } yield locales(i))
  }

  implicit def arbitraryDateTime: Arbitrary[DateTime] =
    Arbitrary(for {
      y <- Gen.choose(-4000, 4000)
      m <- Gen.choose(1, 12)
      d <- Gen.choose(1, 28)
      h <- Gen.choose(0, 23)
      min <- Gen.choose(0, 59)
      s <- Gen.choose(0, 59)
      ms <- Gen.choose(0, 999)
    } yield new DateTime(y, m, d, h, min, s, ms, DateTimeZone.UTC))

  implicit def arbitraryDate: Arbitrary[LocalDate] =
    Arbitrary(Arbitrary.arbitrary[DateTime].map(_.toLocalDate))

  implicit def arbitraryTime: Arbitrary[LocalTime] =
    Arbitrary(Arbitrary.arbitrary[DateTime].map(_.toLocalTime))

  implicit def arbitraryYearMonth: Arbitrary[YearMonth] =
    Arbitrary(Arbitrary.arbitrary[DateTime].map(dt => new YearMonth(dt.getYear, dt.getMonthOfYear)))

  implicit def arbitraryMoney: Arbitrary[Money] =
    Arbitrary(for {
      c <- Arbitrary.arbitrary[Currency]
      i <- Arbitrary.arbitrary[Int]
    } yield Money.make(i, c))

  implicit def arbitraryUUID: Arbitrary[UUID] =
    Arbitrary(for {
      most <- Arbitrary.arbitrary[Long]
      least <- Arbitrary.arbitrary[Long]
    } yield new UUID(most, least))

  implicit val currencyEqual = new Equal[Currency] {
    def equal(c1: Currency, c2: Currency) = c1.getCurrencyCode == c2.getCurrencyCode
  }
  implicit val localeEqual = new Equal[Locale] {
    def equal(l1: Locale, l2: Locale) = l1.toLanguageTag == l2.toLanguageTag
  }
  implicit val uuidEqual = new Equal[UUID] {
    def equal(u1: UUID, u2: UUID) = u1 == u2
  }
  implicit val dateTimeEqual = new Equal[DateTime] {
    def equal(dt1: DateTime, dt2: DateTime) = dt1 == dt2
  }
  implicit val localTimeEqual = new Equal[LocalTime] {
    def equal(dt1: LocalTime, dt2: LocalTime) = dt1 == dt2
  }
  implicit val localDateEqual = new Equal[LocalDate] {
    def equal(dt1: LocalDate, dt2: LocalDate) = dt1 == dt2
  }
  implicit val yearMonthEqual = new Equal[YearMonth] {
    def equal(dt1: YearMonth, dt2: YearMonth) = dt1 == dt2
  }

  private def checkC[C[_]](name: String)(implicit
    jri: FromJSON[C[Int]], jwi: ToJSON[C[Int]], arbi: Arbitrary[C[Int]], eqi: Equal[C[Int]],
    jrs: FromJSON[C[Short]], jws: ToJSON[C[Short]], arbs: Arbitrary[C[Short]], eqs: Equal[C[Short]],
    jrl: FromJSON[C[Long]], jwl: ToJSON[C[Long]], arbl: Arbitrary[C[Long]], eql: Equal[C[Long]],
    jrss: FromJSON[C[String]], jwss: ToJSON[C[String]], arbss: Arbitrary[C[String]], eqss: Equal[C[String]],
    jrf: FromJSON[C[Float]], jwf: ToJSON[C[Float]], arbf: Arbitrary[C[Float]], eqf: Equal[C[Float]],
    jrd: FromJSON[C[Double]], jwd: ToJSON[C[Double]], arbd: Arbitrary[C[Double]], eqd: Equal[C[Double]],
    jrb: FromJSON[C[Boolean]], jwb: ToJSON[C[Boolean]], arbb: Arbitrary[C[Boolean]], eqb: Equal[C[Boolean]]
  ) = {
    property(s"read/write $name of Ints") = Prop.forAll { (l: C[Int]) => check(l) }
    property(s"read/write $name of Shorts") = Prop.forAll { (l: C[Short]) => check(l) }
    property(s"read/write $name of Longs") = Prop.forAll { (l: C[Long]) => check(l) }
    property(s"read/write $name of Strings") = Prop.forAll { (l: C[String]) => check(l) }
    property(s"read/write $name of Floats") = Prop.forAll { (l: C[Float]) => check(l) }
    property(s"read/write $name of Doubles") = Prop.forAll { (l: C[Double]) => check(l) }
    property(s"read/write $name of Booleans") = Prop.forAll { (l: C[Boolean]) => check(l) }
  }

  checkC[List]("List")
  checkC[Vector]("Vector")
  checkC[Set]("Set")
  checkC[NonEmptyList]("NonEmptyList")
  checkC[Option]("Option")
  checkC[({type l[v]=Map[String,v]})#l]("Map")

  property("read/write Unit") = Prop.forAll { (u: Unit) => check(u) }
  property("read/write Currency") = Prop.forAll { (c: Currency) => check(c) }
  property("read/write Money") = Prop.forAll { (m: Money) => check(m) }
  property("read/write Locale") = Prop.forAll { (l: Locale) => check(l) }
  property("read/write UUID") = Prop.forAll { (u: UUID) => check(u) }
  property("read/write DateTime") = Prop.forAll { (u: DateTime) => check(u) }
  property("read/write LocalDate") = Prop.forAll { (u: LocalDate) => check(u) }
  property("read/write LocalTime") = Prop.forAll { (u: LocalTime) => check(u) }
  property("read/write YearMonth") = Prop.forAll { (u: YearMonth) => check(u) }
}
