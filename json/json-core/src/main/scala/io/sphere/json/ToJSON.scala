package io.sphere.json

import cats.data.NonEmptyList
import io.sphere.util.{BaseMoney, Currency, DateTimeFormats, HighPrecisionMoney, Money}
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, DateTimeZone, LocalDate, LocalTime, YearMonth}
import org.json4s.JsonAST._

import java.time
import java.util.{Locale, UUID}
import scala.annotation.implicitNotFound

/** Type class for types that can be written to JSON. */
@implicitNotFound("Could not find an instance of ToJSON for ${A}")
trait ToJSON[@specialized A] extends Serializable {
  def write(value: A): JValue
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONCatsInstances {

  private val emptyJArray = JArray(Nil)
  private val emptyJObject = JObject(Nil)

  @inline def apply[A](implicit instance: ToJSON[A]): ToJSON[A] = instance

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)
  }

  implicit def optionWriter[@specialized A](implicit c: ToJSON[A]): ToJSON[Option[A]] =
    new ToJSON[Option[A]] {
      def write(opt: Option[A]): JValue = opt match {
        case Some(a) => c.write(a)
        case None => JNothing
      }
    }

  implicit def listWriter[@specialized A](implicit w: ToJSON[A]): ToJSON[List[A]] =
    new ToJSON[List[A]] {
      def write(l: List[A]): JValue =
        if (l.isEmpty) emptyJArray
        else JArray(l.map(w.write))
    }

  implicit def nonEmptyListWriter[A](implicit w: ToJSON[A]): ToJSON[NonEmptyList[A]] =
    new ToJSON[NonEmptyList[A]] {
      def write(l: NonEmptyList[A]): JValue = JArray(l.toList.map(w.write))
    }

  implicit def seqWriter[@specialized A](implicit w: ToJSON[A]): ToJSON[Seq[A]] =
    new ToJSON[Seq[A]] {
      def write(s: Seq[A]): JValue =
        if (s.isEmpty) emptyJArray
        else JArray(s.iterator.map(w.write).toList)
    }

  implicit def setWriter[@specialized A](implicit w: ToJSON[A]): ToJSON[Set[A]] =
    new ToJSON[Set[A]] {
      def write(s: Set[A]): JValue =
        if (s.isEmpty) emptyJArray
        else JArray(s.iterator.map(w.write).toList)
    }

  implicit def vectorWriter[@specialized A](implicit w: ToJSON[A]): ToJSON[Vector[A]] =
    new ToJSON[Vector[A]] {
      def write(v: Vector[A]): JValue =
        if (v.isEmpty) emptyJArray
        else JArray(v.iterator.map(w.write).toList)
    }

  implicit val intWriter: ToJSON[Int] = new ToJSON[Int] {
    def write(i: Int): JValue = JLong(i)
  }

  implicit val stringWriter: ToJSON[String] = new ToJSON[String] {
    def write(s: String): JValue = JString(s)
  }

  implicit val bigIntWriter: ToJSON[BigInt] = new ToJSON[BigInt] {
    def write(i: BigInt): JValue = JInt(i)
  }

  implicit val shortWriter: ToJSON[Short] = new ToJSON[Short] {
    def write(s: Short): JValue = JLong(s)
  }

  implicit val longWriter: ToJSON[Long] = new ToJSON[Long] {
    def write(l: Long): JValue = JLong(l)
  }

  implicit val floatWriter: ToJSON[Float] = new ToJSON[Float] {
    def write(f: Float): JValue = JDouble(f)
  }

  implicit val doubleWriter: ToJSON[Double] = new ToJSON[Double] {
    def write(d: Double): JValue = JDouble(d)
  }

  implicit val booleanWriter: ToJSON[Boolean] = new ToJSON[Boolean] {
    def write(b: Boolean): JValue = if (b) JBool.True else JBool.False
  }

  implicit def mapWriter[A: ToJSON]: ToJSON[Map[String, A]] = new ToJSON[Map[String, A]] {
    def write(m: Map[String, A]) =
      if (m.isEmpty) emptyJObject
      else
        JObject(m.iterator.map { case (k, v) =>
          JField(k, toJValue(v))
        }.toList)
  }

  implicit val moneyWriter: ToJSON[Money] = new ToJSON[Money] {
    import Money._

    def write(m: Money): JValue = JObject(
      JField(BaseMoney.TypeField, toJValue(m.`type`)) ::
        JField(CurrencyCodeField, toJValue(m.currency)) ::
        JField(CentAmountField, toJValue(m.centAmount)) ::
        JField(FractionDigitsField, toJValue(m.currency.getDefaultFractionDigits)) ::
        Nil
    )
  }

  implicit val highPrecisionMoneyWriter: ToJSON[HighPrecisionMoney] =
    new ToJSON[HighPrecisionMoney] {
      import HighPrecisionMoney._
      def write(m: HighPrecisionMoney): JValue = JObject(
        JField(BaseMoney.TypeField, toJValue(m.`type`)) ::
          JField(CurrencyCodeField, toJValue(m.currency)) ::
          JField(CentAmountField, toJValue(m.centAmount)) ::
          JField(PreciseAmountField, toJValue(m.preciseAmount)) ::
          JField(FractionDigitsField, toJValue(m.fractionDigits)) ::
          Nil
      )
    }

  implicit val baseMoneyWriter: ToJSON[BaseMoney] = new ToJSON[BaseMoney] {
    def write(m: BaseMoney): JValue = m match {
      case m: Money => moneyWriter.write(m)
      case m: HighPrecisionMoney => highPrecisionMoneyWriter.write(m)
    }
  }

  // This can probably be removed later, but we still need both because of the api-reference repo
  implicit val javaCurrencyWriter: ToJSON[java.util.Currency] = new ToJSON[java.util.Currency] {
    def write(c: java.util.Currency): JValue = toJValue(c.getCurrencyCode)
  }

  implicit val currencyWriter: ToJSON[Currency] = new ToJSON[Currency] {
    def write(c: Currency): JValue = toJValue(c.getCurrencyCode)
  }

  implicit val jValueWriter: ToJSON[JValue] = new ToJSON[JValue] {
    def write(jval: JValue): JValue = jval
  }

  implicit val jObjectWriter: ToJSON[JObject] = new ToJSON[JObject] {
    def write(jObj: JObject): JValue = jObj
  }

  implicit val unitWriter: ToJSON[Unit] = new ToJSON[Unit] {
    def write(u: Unit): JValue = JNothing
  }

  // Joda time

  /** Equivalent to `ISODateTimeFormat.dateTime.print(dt.withZone(UTC))`, but writes the fixed
    * `yyyy-MM-ddTHH:mm:ss.SSSZ` layout straight into a char array instead of going through joda's
    * generic `Composite` printer. That printer accounted for ~13% of RUNNABLE samples in
    * `ToJsonBenchmark`. Equality with joda is pinned by `IsoDateTimePrinterSpec`.
    *
    * ponytail: falls back to joda outside year 0..9999, where the ISO layout is not 4 digits.
    */
  private[json] def printIsoUtc(dt: DateTime): String = {
    val utc = if (dt.getZone eq DateTimeZone.UTC) dt else dt.withZone(DateTimeZone.UTC)
    val year = utc.getYear
    if (year < 0 || year > 9999) ISODateTimeFormat.dateTime.print(utc)
    else {
      val b = new Array[Char](24)
      put2(b, 0, year / 100)
      put2(b, 2, year % 100)
      b(4) = '-'
      put2(b, 5, utc.getMonthOfYear)
      b(7) = '-'
      put2(b, 8, utc.getDayOfMonth)
      b(10) = 'T'
      put2(b, 11, utc.getHourOfDay)
      b(13) = ':'
      put2(b, 14, utc.getMinuteOfHour)
      b(16) = ':'
      put2(b, 17, utc.getSecondOfMinute)
      b(19) = '.'
      val ms = utc.getMillisOfSecond
      b(20) = ('0' + ms / 100).toChar
      put2(b, 21, ms % 100)
      b(23) = 'Z'
      new String(b)
    }
  }

  private def put2(b: Array[Char], i: Int, v: Int): Unit = {
    b(i) = ('0' + v / 10).toChar
    b(i + 1) = ('0' + v % 10).toChar
  }

  implicit val dateTimeWriter: ToJSON[DateTime] = new ToJSON[DateTime] {
    def write(dt: DateTime): JValue = JString(printIsoUtc(dt))
  }

  implicit val timeWriter: ToJSON[LocalTime] = new ToJSON[LocalTime] {
    def write(lt: LocalTime): JValue = JString(ISODateTimeFormat.time.print(lt))
  }

  implicit val dateWriter: ToJSON[LocalDate] = new ToJSON[LocalDate] {
    def write(ld: LocalDate): JValue = JString(ISODateTimeFormat.date.print(ld))
  }

  implicit val yearMonthWriter: ToJSON[YearMonth] = new ToJSON[YearMonth] {
    def write(ym: YearMonth): JValue = JString(ISODateTimeFormat.yearMonth().print(ym))
  }

  // java.time
  implicit val javaInstantWriter: ToJSON[time.Instant] = new ToJSON[time.Instant] {
    def write(value: time.Instant): JValue = JString(DateTimeFormats.format(value))
  }

  implicit val javaTimeWriter: ToJSON[time.LocalTime] = new ToJSON[time.LocalTime] {
    def write(value: time.LocalTime): JValue = JString(DateTimeFormats.format(value))
  }

  implicit val javaDateWriter: ToJSON[time.LocalDate] = new ToJSON[time.LocalDate] {
    def write(value: time.LocalDate): JValue = JString(DateTimeFormats.format(value))
  }

  implicit val javaYearMonth: ToJSON[time.YearMonth] = new ToJSON[time.YearMonth] {
    def write(value: time.YearMonth): JValue = JString(JavaYearMonthFormatter.format(value))
  }

  implicit val uuidWriter: ToJSON[UUID] = new ToJSON[UUID] {
    def write(uuid: UUID): JValue = JString(uuid.toString)
  }

  implicit val localeWriter: ToJSON[Locale] = new ToJSON[Locale] {
    def write(locale: Locale): JValue = JString(locale.toLanguageTag)
  }

  implicit def eitherWriter[A: ToJSON, B: ToJSON]: ToJSON[Either[A, B]] = new ToJSON[Either[A, B]] {
    def write(e: Either[A, B]): JValue = e match {
      case Left(l) => toJValue(l)
      case Right(r) => toJValue(r)
    }
  }
}
