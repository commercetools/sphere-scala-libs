package io.sphere.json

import scala.language.higherKinds
import io.sphere.util.Money
import java.util.{Currency, Locale, UUID}

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.eq._
import org.joda.time._
import org.scalacheck._
import java.time

import scala.math.BigDecimal.RoundingMode

object JSONProperties extends Properties("JSON") {
  private def check[A: FromJSON: ToJSON: Eq](a: A): Boolean = {
    val json = s"""[${toJSON(a)}]"""
    val result = fromJSON[Seq[A]](json).toOption.map(_.head).get
    val r = result === a
    if (!r) println(s"result: $result - expected: $a")
    r
  }

  implicit def arbitraryVector[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(_.toVector))

  implicit def arbitraryNEL[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary(for {
      a <- Arbitrary.arbitrary[A]
      l <- Arbitrary.arbitrary[List[A]]
    } yield NonEmptyList(a, l))

  implicit def arbitraryCurrency: Arbitrary[Currency] =
    Arbitrary(Gen
      .oneOf(Currency.getInstance("EUR"), Currency.getInstance("USD"), Currency.getInstance("JPY")))

  implicit def arbitraryLocale: Arbitrary[Locale] = {
    // Filter because OS X thinks that 'C' and 'POSIX' are valid locales...
    val locales = Locale.getAvailableLocales().filter(_.toLanguageTag() != "und")
    Arbitrary(for {
      i <- Gen.choose(0, locales.length - 1)
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

  // generate dates between years -4000 and +4000
  implicit val javaInstant: Arbitrary[time.Instant] =
    Arbitrary(Gen.choose(-188395027761000L, 64092207599999L).map(time.Instant.ofEpochMilli(_)))

  implicit val javaLocalTime: Arbitrary[time.LocalTime] = Arbitrary(
    Gen.choose(0, 3600 * 24).map(time.LocalTime.ofSecondOfDay(_)))

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
    } yield Money.fromDecimalAmount(i, c)(RoundingMode.HALF_EVEN))

  implicit def arbitraryUUID: Arbitrary[UUID] =
    Arbitrary(for {
      most <- Arbitrary.arbitrary[Long]
      least <- Arbitrary.arbitrary[Long]
    } yield new UUID(most, least))

  implicit val currencyEqual: Eq[Currency] = new Eq[Currency] {
    def eqv(c1: Currency, c2: Currency) = c1.getCurrencyCode == c2.getCurrencyCode
  }
  implicit val localeEqual: Eq[Locale] = new Eq[Locale] {
    def eqv(l1: Locale, l2: Locale) = l1.toLanguageTag == l2.toLanguageTag
  }
  implicit val moneyEqual: Eq[Money] = new Eq[Money] {
    override def eqv(x: Money, y: Money): Boolean = x == y
  }
  implicit val dateTimeEqual: Eq[DateTime] = new Eq[DateTime] {
    def eqv(dt1: DateTime, dt2: DateTime) = dt1 == dt2
  }
  implicit val localTimeEqual: Eq[LocalTime] = new Eq[LocalTime] {
    def eqv(dt1: LocalTime, dt2: LocalTime) = dt1 == dt2
  }
  implicit val localDateEqual: Eq[LocalDate] = new Eq[LocalDate] {
    def eqv(dt1: LocalDate, dt2: LocalDate) = dt1 == dt2
  }
  implicit val yearMonthEqual: Eq[YearMonth] = new Eq[YearMonth] {
    def eqv(dt1: YearMonth, dt2: YearMonth) = dt1 == dt2
  }
  implicit val javaInstantEqual: Eq[time.Instant] = Eq.fromUniversalEquals
  implicit val javaLocalDateEqual: Eq[time.LocalDate] = Eq.fromUniversalEquals
  implicit val javaLocalTimeEqual: Eq[time.LocalTime] = Eq.fromUniversalEquals
  implicit val javaYearMonthEqual: Eq[time.YearMonth] = Eq.fromUniversalEquals

  private def checkC[C[_]](name: String)(implicit
      jri: FromJSON[C[Int]],
      jwi: ToJSON[C[Int]],
      arbi: Arbitrary[C[Int]],
      eqi: Eq[C[Int]],
      jrs: FromJSON[C[Short]],
      jws: ToJSON[C[Short]],
      arbs: Arbitrary[C[Short]],
      eqs: Eq[C[Short]],
      jrl: FromJSON[C[Long]],
      jwl: ToJSON[C[Long]],
      arbl: Arbitrary[C[Long]],
      eql: Eq[C[Long]],
      jrss: FromJSON[C[String]],
      jwss: ToJSON[C[String]],
      arbss: Arbitrary[C[String]],
      eqss: Eq[C[String]],
      jrf: FromJSON[C[Float]],
      jwf: ToJSON[C[Float]],
      arbf: Arbitrary[C[Float]],
      eqf: Eq[C[Float]],
      jrd: FromJSON[C[Double]],
      jwd: ToJSON[C[Double]],
      arbd: Arbitrary[C[Double]],
      eqd: Eq[C[Double]],
      jrb: FromJSON[C[Boolean]],
      jwb: ToJSON[C[Boolean]],
      arbb: Arbitrary[C[Boolean]],
      eqb: Eq[C[Boolean]]
  ) = {
    property(s"read/write $name of Ints") = Prop.forAll((l: C[Int]) => check(l))
    property(s"read/write $name of Shorts") = Prop.forAll((l: C[Short]) => check(l))
    property(s"read/write $name of Longs") = Prop.forAll((l: C[Long]) => check(l))
    property(s"read/write $name of Strings") = Prop.forAll((l: C[String]) => check(l))
    property(s"read/write $name of Floats") = Prop.forAll((l: C[Float]) => check(l))
    property(s"read/write $name of Doubles") = Prop.forAll((l: C[Double]) => check(l))
    property(s"read/write $name of Booleans") = Prop.forAll((l: C[Boolean]) => check(l))
  }

  checkC[List]("List")
  checkC[Vector]("Vector")
  checkC[Set]("Set")
  checkC[NonEmptyList]("NonEmptyList")
  checkC[Option]("Option")
  checkC[({ type l[v] = Map[String, v] })#l]("Map")

  property("read/write Unit") = Prop.forAll((u: Unit) => check(u))
  property("read/write Currency") = Prop.forAll((c: Currency) => check(c))
  property("read/write Money") = Prop.forAll((m: Money) => check(m))
  property("read/write Locale") = Prop.forAll((l: Locale) => check(l))
  property("read/write UUID") = Prop.forAll((u: UUID) => check(u))
  property("read/write DateTime") = Prop.forAll((u: DateTime) => check(u))
  property("read/write LocalDate") = Prop.forAll((u: LocalDate) => check(u))
  property("read/write LocalTime") = Prop.forAll((u: LocalTime) => check(u))
  property("read/write YearMonth") = Prop.forAll((u: YearMonth) => check(u))
  property("read/write java.time.Instant") = Prop.forAll((i: time.Instant) => check(i))
  property("read/write java.time.LocalDate") = Prop.forAll((d: time.LocalDate) => check(d))
  property("read/write java.time.LocalTime") = Prop.forAll((t: time.LocalTime) => check(t))
  property("read/write java.time.YearMonth") = Prop.forAll((ym: time.YearMonth) => check(ym))
}
