package io.sphere.json

import cats.data.NonEmptyList
import java.util.{Currency, Locale, UUID}

import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import org.json4s.JsonAST._
import org.joda.time._
import org.joda.time.format.ISODateTimeFormat

import scala.annotation.implicitNotFound

/** Type class for types that can be written to JSON. */
@implicitNotFound("Could not find an instance of ToJSON for ${A}")
trait ToJSON[@specialized A] extends Serializable {
  def write(value: A): JValue
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONInstances {

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
          JField(PreciseAmountField, toJValue(m.preciseAmountAsLong)) ::
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

  implicit val dateTimeWriter: ToJSON[DateTime] = new ToJSON[DateTime] {
    def write(dt: DateTime): JValue = JString(
      ISODateTimeFormat.dateTime.print(dt.withZone(DateTimeZone.UTC)))
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
