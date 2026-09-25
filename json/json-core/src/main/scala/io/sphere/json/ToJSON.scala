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

  /** Writes `value` straight into `sink`, skipping the intermediate `JValue` tree. This is the fast
    * path — it is ~2.5x the throughput of `write` plus Jackson, see
    * `docs/json-serialization-perf.md`.
    *
    * The default routes through `write`, so existing instances keep working unchanged; override it
    * wherever the cost matters.
    */
  def writeTo(value: A, sink: JsonSink): Unit = sink.jValue(write(value))

  /** True when `writeTo` would emit nothing at all — the `JNothing` "absent field" sentinel.
    *
    * Callers must consult this *before* emitting a separator and field name, because a sink cannot
    * take them back. The default answers by building the `JValue`, which means instances that
    * override neither this nor `writeTo` compute their value twice on the sink path. Override it
    * (or extend `ToJSON.Always`) to avoid that.
    */
  def writesNothing(value: A): Boolean = write(value) eq JNothing
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONCatsInstances {

  private val emptyJArray = JArray(Nil)
  private val emptyJObject = JObject(Nil)

  @inline def apply[A](implicit instance: ToJSON[A]): ToJSON[A] = instance

  /** Base class for writers that always emit a value, i.e. never the `JNothing` sentinel. Fixing
    * `writesNothing` here keeps the check off the derived write path for the common case.
    */
  abstract class Always[@specialized A] extends ToJSON[A] {
    final override def writesNothing(value: A): Boolean = false
  }

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)
  }

  /** Writes the elements of `xs` as a JSON array, skipping any that write nothing — which is what
    * the `JValue` path did implicitly, since json4s drops `JNothing` at render time.
    *
    * Walking `Vector` through its iterator and everything else through `foreach` is worth ~25%
    * either way on 100k elements; with the JValue tree gone, the traversal is a real share of the
    * remaining work. See `docs/json-serialization-perf.md`.
    */
  private def writeElements[A](
      xs: scala.collection.Iterable[A],
      w: ToJSON[A],
      s: JsonSink): Unit = {
    s.ch('[')
    var wrote = false
    xs match {
      case v: Vector[A] =>
        val it = v.iterator
        while (it.hasNext) {
          val a = it.next()
          if (!w.writesNothing(a)) {
            if (wrote) s.ch(',') else wrote = true
            w.writeTo(a, s)
          }
        }
      case _ =>
        xs.foreach { a =>
          if (!w.writesNothing(a)) {
            if (wrote) s.ch(',') else wrote = true
            w.writeTo(a, s)
          }
        }
    }
    s.ch(']')
  }

  implicit def optionWriter[@specialized A](implicit c: ToJSON[A]): ToJSON[Option[A]] =
    new ToJSON[Option[A]] {
      def write(opt: Option[A]): JValue = opt match {
        case Some(a) => c.write(a)
        case None => JNothing
      }
      override def writeTo(opt: Option[A], s: JsonSink): Unit = opt match {
        case Some(a) => c.writeTo(a, s)
        case None => ()
      }
      override def writesNothing(opt: Option[A]): Boolean = opt match {
        case Some(a) => c.writesNothing(a)
        case None => true
      }
    }

  implicit def listWriter[@specialized A](implicit w: ToJSON[A]): ToJSON[List[A]] =
    new Always[List[A]] {
      def write(l: List[A]): JValue =
        if (l.isEmpty) emptyJArray
        else JArray(l.map(w.write))
      override def writeTo(l: List[A], s: JsonSink): Unit = writeElements(l, w, s)
    }

  implicit def nonEmptyListWriter[A](implicit w: ToJSON[A]): ToJSON[NonEmptyList[A]] =
    new Always[NonEmptyList[A]] {
      def write(l: NonEmptyList[A]): JValue = JArray(l.toList.map(w.write))
      override def writeTo(l: NonEmptyList[A], s: JsonSink): Unit =
        writeElements(l.toList, w, s)
    }

  implicit def seqWriter[@specialized A](implicit w: ToJSON[A]): ToJSON[Seq[A]] =
    new Always[Seq[A]] {
      def write(s: Seq[A]): JValue =
        if (s.isEmpty) emptyJArray
        else JArray(s.iterator.map(w.write).toList)
      override def writeTo(v: Seq[A], s: JsonSink): Unit = writeElements(v, w, s)
    }

  implicit def setWriter[@specialized A](implicit w: ToJSON[A]): ToJSON[Set[A]] =
    new Always[Set[A]] {
      def write(s: Set[A]): JValue =
        if (s.isEmpty) emptyJArray
        else JArray(s.iterator.map(w.write).toList)
      override def writeTo(v: Set[A], s: JsonSink): Unit = writeElements(v, w, s)
    }

  implicit def vectorWriter[@specialized A](implicit w: ToJSON[A]): ToJSON[Vector[A]] =
    new Always[Vector[A]] {
      def write(v: Vector[A]): JValue =
        if (v.isEmpty) emptyJArray
        else JArray(v.iterator.map(w.write).toList)
      override def writeTo(v: Vector[A], s: JsonSink): Unit = writeElements(v, w, s)
    }

  implicit val intWriter: ToJSON[Int] = new Always[Int] {
    def write(i: Int): JValue = JLong(i)
    override def writeTo(i: Int, s: JsonSink): Unit = s.long(i.toLong)
  }

  implicit val stringWriter: ToJSON[String] = new Always[String] {
    def write(s: String): JValue = JString(s)
    override def writeTo(v: String, s: JsonSink): Unit = s.string(v)
  }

  implicit val bigIntWriter: ToJSON[BigInt] = new Always[BigInt] {
    def write(i: BigInt): JValue = JInt(i)
    override def writeTo(i: BigInt, s: JsonSink): Unit = s.raw(i.bigInteger.toString)
  }

  implicit val shortWriter: ToJSON[Short] = new Always[Short] {
    def write(s: Short): JValue = JLong(s)
    override def writeTo(v: Short, s: JsonSink): Unit = s.long(v.toLong)
  }

  implicit val longWriter: ToJSON[Long] = new Always[Long] {
    def write(l: Long): JValue = JLong(l)
    override def writeTo(l: Long, s: JsonSink): Unit = s.long(l)
  }

  implicit val floatWriter: ToJSON[Float] = new Always[Float] {
    def write(f: Float): JValue = JDouble(f)
    override def writeTo(f: Float, s: JsonSink): Unit = s.double(f.toDouble)
  }

  implicit val doubleWriter: ToJSON[Double] = new Always[Double] {
    def write(d: Double): JValue = JDouble(d)
    override def writeTo(d: Double, s: JsonSink): Unit = s.double(d)
  }

  implicit val booleanWriter: ToJSON[Boolean] = new Always[Boolean] {
    def write(b: Boolean): JValue = if (b) JBool.True else JBool.False
    override def writeTo(b: Boolean, s: JsonSink): Unit = s.boolean(b)
  }

  implicit def mapWriter[A: ToJSON]: ToJSON[Map[String, A]] = new Always[Map[String, A]] {
    private val w = ToJSON[A]
    def write(m: Map[String, A]) =
      if (m.isEmpty) emptyJObject
      else
        JObject(m.iterator.map { case (k, v) =>
          JField(k, toJValue(v))
        }.toList)
    override def writeTo(m: Map[String, A], s: JsonSink): Unit = {
      s.ch('{')
      var wrote = false
      m.foreach { kv =>
        val v = kv._2
        if (!w.writesNothing(v)) {
          if (wrote) s.ch(',') else wrote = true
          s.string(kv._1)
          s.ch(':')
          w.writeTo(v, s)
        }
      }
      s.ch('}')
    }
  }

  implicit val moneyWriter: ToJSON[Money] = new Always[Money] {
    import Money._

    private val typePrefix = JsonSink.fieldPrefix(BaseMoney.TypeField)
    private val currencyPrefix = "," + JsonSink.fieldPrefix(CurrencyCodeField)
    private val centAmountPrefix = "," + JsonSink.fieldPrefix(CentAmountField)
    private val fractionDigitsPrefix = "," + JsonSink.fieldPrefix(FractionDigitsField)

    def write(m: Money): JValue = JObject(
      JField(BaseMoney.TypeField, toJValue(m.`type`)) ::
        JField(CurrencyCodeField, toJValue(m.currency)) ::
        JField(CentAmountField, toJValue(m.centAmount)) ::
        JField(FractionDigitsField, toJValue(m.currency.getDefaultFractionDigits)) ::
        Nil
    )

    override def writeTo(m: Money, s: JsonSink): Unit = {
      s.ch('{')
      s.raw(typePrefix); s.string(m.`type`)
      s.raw(currencyPrefix); s.string(m.currency.getCurrencyCode)
      s.raw(centAmountPrefix); s.long(m.centAmount)
      s.raw(fractionDigitsPrefix); s.long(m.currency.getDefaultFractionDigits.toLong)
      s.ch('}')
    }
  }

  implicit val highPrecisionMoneyWriter: ToJSON[HighPrecisionMoney] =
    new Always[HighPrecisionMoney] {
      import HighPrecisionMoney._

      private val typePrefix = JsonSink.fieldPrefix(BaseMoney.TypeField)
      private val currencyPrefix = "," + JsonSink.fieldPrefix(CurrencyCodeField)
      private val centAmountPrefix = "," + JsonSink.fieldPrefix(CentAmountField)
      private val preciseAmountPrefix = "," + JsonSink.fieldPrefix(PreciseAmountField)
      private val fractionDigitsPrefix = "," + JsonSink.fieldPrefix(FractionDigitsField)

      def write(m: HighPrecisionMoney): JValue = JObject(
        JField(BaseMoney.TypeField, toJValue(m.`type`)) ::
          JField(CurrencyCodeField, toJValue(m.currency)) ::
          JField(CentAmountField, toJValue(m.centAmount)) ::
          JField(PreciseAmountField, toJValue(m.preciseAmount)) ::
          JField(FractionDigitsField, toJValue(m.fractionDigits)) ::
          Nil
      )

      override def writeTo(m: HighPrecisionMoney, s: JsonSink): Unit = {
        s.ch('{')
        s.raw(typePrefix); s.string(m.`type`)
        s.raw(currencyPrefix); s.string(m.currency.getCurrencyCode)
        s.raw(centAmountPrefix); s.long(m.centAmount)
        s.raw(preciseAmountPrefix); s.long(m.preciseAmount)
        s.raw(fractionDigitsPrefix); s.long(m.fractionDigits.toLong)
        s.ch('}')
      }
    }

  implicit val baseMoneyWriter: ToJSON[BaseMoney] = new Always[BaseMoney] {
    def write(m: BaseMoney): JValue = m match {
      case m: Money => moneyWriter.write(m)
      case m: HighPrecisionMoney => highPrecisionMoneyWriter.write(m)
    }
    override def writeTo(m: BaseMoney, s: JsonSink): Unit = m match {
      case m: Money => moneyWriter.writeTo(m, s)
      case m: HighPrecisionMoney => highPrecisionMoneyWriter.writeTo(m, s)
    }
  }

  // This can probably be removed later, but we still need both because of the api-reference repo
  implicit val javaCurrencyWriter: ToJSON[java.util.Currency] = new Always[java.util.Currency] {
    def write(c: java.util.Currency): JValue = toJValue(c.getCurrencyCode)
    override def writeTo(c: java.util.Currency, s: JsonSink): Unit = s.string(c.getCurrencyCode)
  }

  implicit val currencyWriter: ToJSON[Currency] = new Always[Currency] {
    def write(c: Currency): JValue = toJValue(c.getCurrencyCode)
    override def writeTo(c: Currency, s: JsonSink): Unit = s.string(c.getCurrencyCode)
  }

  implicit val jValueWriter: ToJSON[JValue] = new ToJSON[JValue] {
    def write(jval: JValue): JValue = jval
    override def writeTo(jval: JValue, s: JsonSink): Unit = s.jValue(jval)
    override def writesNothing(jval: JValue): Boolean = jval eq JNothing
  }

  implicit val jObjectWriter: ToJSON[JObject] = new Always[JObject] {
    def write(jObj: JObject): JValue = jObj
    override def writeTo(jObj: JObject, s: JsonSink): Unit = s.jValue(jObj)
  }

  implicit val unitWriter: ToJSON[Unit] = new ToJSON[Unit] {
    def write(u: Unit): JValue = JNothing
    override def writeTo(u: Unit, s: JsonSink): Unit = ()
    override def writesNothing(u: Unit): Boolean = true
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

  implicit val dateTimeWriter: ToJSON[DateTime] = new Always[DateTime] {
    def write(dt: DateTime): JValue = JString(printIsoUtc(dt))
    override def writeTo(dt: DateTime, s: JsonSink): Unit = s.isoUtc(dt)
  }

  implicit val timeWriter: ToJSON[LocalTime] = new Always[LocalTime] {
    def write(lt: LocalTime): JValue = JString(ISODateTimeFormat.time.print(lt))
    override def writeTo(lt: LocalTime, s: JsonSink): Unit =
      s.string(ISODateTimeFormat.time.print(lt))
  }

  implicit val dateWriter: ToJSON[LocalDate] = new Always[LocalDate] {
    def write(ld: LocalDate): JValue = JString(ISODateTimeFormat.date.print(ld))
    override def writeTo(ld: LocalDate, s: JsonSink): Unit =
      s.string(ISODateTimeFormat.date.print(ld))
  }

  implicit val yearMonthWriter: ToJSON[YearMonth] = new Always[YearMonth] {
    def write(ym: YearMonth): JValue = JString(ISODateTimeFormat.yearMonth().print(ym))
    override def writeTo(ym: YearMonth, s: JsonSink): Unit =
      s.string(ISODateTimeFormat.yearMonth().print(ym))
  }

  // java.time
  implicit val javaInstantWriter: ToJSON[time.Instant] = new Always[time.Instant] {
    def write(value: time.Instant): JValue = JString(DateTimeFormats.format(value))
    override def writeTo(value: time.Instant, s: JsonSink): Unit =
      s.string(DateTimeFormats.format(value))
  }

  implicit val javaTimeWriter: ToJSON[time.LocalTime] = new Always[time.LocalTime] {
    def write(value: time.LocalTime): JValue = JString(DateTimeFormats.format(value))
    override def writeTo(value: time.LocalTime, s: JsonSink): Unit =
      s.string(DateTimeFormats.format(value))
  }

  implicit val javaDateWriter: ToJSON[time.LocalDate] = new Always[time.LocalDate] {
    def write(value: time.LocalDate): JValue = JString(DateTimeFormats.format(value))
    override def writeTo(value: time.LocalDate, s: JsonSink): Unit =
      s.string(DateTimeFormats.format(value))
  }

  implicit val javaYearMonth: ToJSON[time.YearMonth] = new Always[time.YearMonth] {
    def write(value: time.YearMonth): JValue = JString(JavaYearMonthFormatter.format(value))
    override def writeTo(value: time.YearMonth, s: JsonSink): Unit =
      s.string(JavaYearMonthFormatter.format(value))
  }

  implicit val uuidWriter: ToJSON[UUID] = new Always[UUID] {
    def write(uuid: UUID): JValue = JString(uuid.toString)
    override def writeTo(uuid: UUID, s: JsonSink): Unit = s.string(uuid.toString)
  }

  implicit val localeWriter: ToJSON[Locale] = new Always[Locale] {
    def write(locale: Locale): JValue = JString(locale.toLanguageTag)
    override def writeTo(locale: Locale, s: JsonSink): Unit = s.string(locale.toLanguageTag)
  }

  implicit def eitherWriter[A: ToJSON, B: ToJSON]: ToJSON[Either[A, B]] =
    new ToJSON[Either[A, B]] {
      private val lw = ToJSON[A]
      private val rw = ToJSON[B]
      def write(e: Either[A, B]): JValue = e match {
        case Left(l) => toJValue(l)
        case Right(r) => toJValue(r)
      }
      override def writeTo(e: Either[A, B], s: JsonSink): Unit = e match {
        case Left(l) => lw.writeTo(l, s)
        case Right(r) => rw.writeTo(r, s)
      }
      override def writesNothing(e: Either[A, B]): Boolean = e match {
        case Left(l) => lw.writesNothing(l)
        case Right(r) => rw.writesNothing(r)
      }
    }
}
