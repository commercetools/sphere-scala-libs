package io.sphere.json

import cats.data.NonEmptyList
import io.sphere.json.generic.JSONTypeSwitch.ToFormatters
import io.sphere.util.{BaseMoney, DateTimeFormats, HighPrecisionMoney, Money}
import org.joda.time.*
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonAST.*

import java.time
import java.util.{Currency, Locale, UUID}

/** Type class for types that can be written to JSON. */
trait ToJSON[A] extends Serializable {
  def write(value: A): JValue

  // Filled automatically for traits
  // I decided to not use option, because it's not an internal type anyway and
  // on traits it's always filled
  // on case classes it's always null
  // So there's not a lot of reasons to check for it runtime in most cases.
  val toFormatters: ToFormatters = null
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONCatsInstances with generic.DeriveToJSON {

  private val emptyJArray = JArray(Nil)
  private val emptyJObject = JObject(Nil)

  inline def apply[A](using instance: ToJSON[A]): ToJSON[A] = instance
  inline def apply[A: JSON]: ToJSON[A] = summon[ToJSON[A]]

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue, toFs: ToFormatters = null): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)

    override val toFormatters: ToFormatters = toFs
  }

  given optionWriter[A](using c: ToJSON[A]): ToJSON[Option[A]] with {
    def write(opt: Option[A]): JValue = opt match {
      case Some(a) => c.write(a)
      case None => JNothing
    }
  }

  given listWriter[A](using w: ToJSON[A]): ToJSON[List[A]] with {
    def write(l: List[A]): JValue =
      if (l.isEmpty) emptyJArray
      else JArray(l.map(w.write))
  }

  given nonEmptyListWriter[A](using w: ToJSON[A]): ToJSON[NonEmptyList[A]] with {
    def write(l: NonEmptyList[A]): JValue = JArray(l.toList.map(w.write))
  }

  given seqWriter[A](using w: ToJSON[A]): ToJSON[Seq[A]] with {
    def write(s: Seq[A]): JValue =
      if (s.isEmpty) emptyJArray
      else JArray(s.iterator.map(w.write).toList)
  }

  given setWriter[A](using w: ToJSON[A]): ToJSON[Set[A]] with {
    def write(s: Set[A]): JValue =
      if (s.isEmpty) emptyJArray
      else JArray(s.iterator.map(w.write).toList)
  }

  given vectorWriter[A](using w: ToJSON[A]): ToJSON[Vector[A]] with {
    def write(v: Vector[A]): JValue =
      if (v.isEmpty) emptyJArray
      else JArray(v.iterator.map(w.write).toList)
  }

  given intWriter: ToJSON[Int] with {
    def write(i: Int): JValue = JLong(i)
  }

  given stringWriter: ToJSON[String] with {
    def write(s: String): JValue = JString(s)
  }

  given bigIntWriter: ToJSON[BigInt] with {
    def write(i: BigInt): JValue = JInt(i)
  }

  given shortWriter: ToJSON[Short] with {
    def write(s: Short): JValue = JLong(s)
  }

  given longWriter: ToJSON[Long] with {
    def write(l: Long): JValue = JLong(l)
  }

  given floatWriter: ToJSON[Float] with {
    def write(f: Float): JValue = JDouble(f)
  }

  given doubleWriter: ToJSON[Double] with {
    def write(d: Double): JValue = JDouble(d)
  }

  given booleanWriter: ToJSON[Boolean] with {
    def write(b: Boolean): JValue = if (b) JBool.True else JBool.False
  }

  given mapWriter[A](using ToJSON[A]): ToJSON[Map[String, A]] with {
    def write(m: Map[String, A]) =
      if (m.isEmpty) emptyJObject
      else
        JObject(m.iterator.map { case (k, v) =>
          JField(k, toJValue(v))
        }.toList)
  }

  given moneyWriter: ToJSON[Money] with {

    import Money.*

    def write(m: Money): JValue = JObject(
      JField(BaseMoney.TypeField, toJValue(m.`type`)) ::
        JField(CurrencyCodeField, toJValue(m.currency)) ::
        JField(CentAmountField, toJValue(m.centAmount)) ::
        JField(FractionDigitsField, toJValue(m.currency.getDefaultFractionDigits)) ::
        Nil
    )
  }

  given highPrecisionMoneyWriter: ToJSON[HighPrecisionMoney] with {

    import HighPrecisionMoney.*

    def write(m: HighPrecisionMoney): JValue = JObject(
      JField(BaseMoney.TypeField, toJValue(m.`type`)) ::
        JField(CurrencyCodeField, toJValue(m.currency)) ::
        JField(CentAmountField, toJValue(m.centAmount)) ::
        JField(PreciseAmountField, toJValue(m.preciseAmount)) ::
        JField(FractionDigitsField, toJValue(m.fractionDigits)) ::
        Nil
    )
  }

  given baseMoneyWriter: ToJSON[BaseMoney] with {
    def write(m: BaseMoney): JValue = m match {
      case m: Money => moneyWriter.write(m)
      case m: HighPrecisionMoney => highPrecisionMoneyWriter.write(m)
    }
  }

  given currencyWriter: ToJSON[Currency] with {
    def write(c: Currency): JValue = toJValue(c.getCurrencyCode)
  }

  given jValueWriter: ToJSON[JValue] with {
    def write(jval: JValue): JValue = jval
  }

  given jObjectWriter: ToJSON[JObject] with {
    def write(jObj: JObject): JValue = jObj
  }

  given unitWriter: ToJSON[Unit] with {
    def write(u: Unit): JValue = JNothing
  }

  // Joda time
  given dateTimeWriter: ToJSON[DateTime] with {
    def write(dt: DateTime): JValue = JString(
      ISODateTimeFormat.dateTime.print(dt.withZone(DateTimeZone.UTC)))
  }

  given timeWriter: ToJSON[LocalTime] with {
    def write(lt: LocalTime): JValue = JString(ISODateTimeFormat.time.print(lt))
  }

  given dateWriter: ToJSON[LocalDate] with {
    def write(ld: LocalDate): JValue = JString(ISODateTimeFormat.date.print(ld))
  }

  given yearMonthWriter: ToJSON[YearMonth] with {
    def write(ym: YearMonth): JValue = JString(ISODateTimeFormat.yearMonth().print(ym))
  }

  // java.time
  given javaInstantWriter: ToJSON[time.Instant] with {
    def write(value: time.Instant): JValue = JString(DateTimeFormats.format(value))
  }

  given javaTimeWriter: ToJSON[time.LocalTime] with {
    def write(value: time.LocalTime): JValue = JString(DateTimeFormats.format(value))
  }

  given javaDateWriter: ToJSON[time.LocalDate] with {
    def write(value: time.LocalDate): JValue = JString(DateTimeFormats.format(value))
  }

  given javaYearMonth: ToJSON[time.YearMonth] with {
    def write(value: time.YearMonth): JValue = JString(JavaYearMonthFormatter.format(value))
  }

  given uuidWriter: ToJSON[UUID] with {
    def write(uuid: UUID): JValue = JString(uuid.toString)
  }

  given localeWriter: ToJSON[Locale] with {
    def write(locale: Locale): JValue = JString(locale.toLanguageTag)
  }

  given eitherWriter[A, B](using ToJSON[A], ToJSON[B]): ToJSON[Either[A, B]] with {
    def write(e: Either[A, B]): JValue = e match {
      case Left(l) => toJValue(l)
      case Right(r) => toJValue(r)
    }
  }
}
