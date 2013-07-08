package io.sphere.json

import scalaz.NonEmptyList
import scalaz.syntax.std.option._
import scala.collection.breakOut

import java.util.{ Locale, Currency, UUID }

import io.sphere.util.Money

import net.liftweb.json.JsonAST._
import org.joda.time._
import org.joda.time.format.ISODateTimeFormat

/** Type class for types that can be written to JSON. */
trait ToJSON[A] {
  def write(value: A): JValue
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON {

  implicit def optionWriter[A](implicit c: ToJSON[A]): ToJSON[Option[A]] = new ToJSON[Option[A]] {
    def write(opt: Option[A]): JValue = opt.some(c.write).none(JNothing)
  }

  implicit def listWriter[A](implicit w: ToJSON[A]): ToJSON[List[A]] = new ToJSON[List[A]] {
    def write(l: List[A]): JValue = JArray(l map w.write)
  }

  implicit def nonEmptyListWriter[A](implicit w: ToJSON[A]): ToJSON[NonEmptyList[A]] = new ToJSON[NonEmptyList[A]] {
    def write(l: NonEmptyList[A]): JValue = JArray(l.list map w.write)
  }

  implicit def seqWriter[A](implicit w: ToJSON[A]): ToJSON[Seq[A]] = new ToJSON[Seq[A]] {
    def write(s: Seq[A]): JValue = JArray(s.map(w.write)(breakOut))
  }

  implicit def setWriter[A](implicit w: ToJSON[A]): ToJSON[Set[A]] = new ToJSON[Set[A]] {
    def write(s: Set[A]): JValue = JArray(s.map(w.write)(breakOut))
  }

  implicit def vectorWriter[A](implicit w: ToJSON[A]): ToJSON[Vector[A]] = new ToJSON[Vector[A]] {
    def write(v: Vector[A]): JValue = JArray(v.map(w.write)(breakOut))
  }

  implicit val intWriter: ToJSON[Int] = new ToJSON[Int] {
    def write(i: Int): JValue = JInt(i)
  }

  implicit val stringWriter: ToJSON[String] = new ToJSON[String] {
    def write(s: String): JValue = JString(s)
  }

  implicit val bigIntWriter: ToJSON[BigInt] = new ToJSON[BigInt] {
    def write(i: BigInt): JValue = JInt(i)
  }

  implicit val shortWriter: ToJSON[Short] = new ToJSON[Short] {
    def write(s: Short): JValue = JInt(s)
  }

  implicit val longWriter: ToJSON[Long] = new ToJSON[Long] {
    def write(l: Long): JValue = JInt(l)
  }

  implicit val floatWriter: ToJSON[Float] = new ToJSON[Float] {
    def write(f: Float): JValue = JDouble(f)
  }

  implicit val doubleWriter: ToJSON[Double] = new ToJSON[Double] {
    def write(d: Double): JValue = JDouble(d)
  }

  implicit val booleanWriter: ToJSON[Boolean] = new ToJSON[Boolean] {
    def write(b: Boolean): JValue = JBool(b)
  }

  implicit def mapWriter[A: ToJSON]: ToJSON[Map[String, A]] = new ToJSON[Map[String, A]] {
    def write(m: Map[String, A]) = JObject(m.map { case (k, v) => JField(k, toJValue(v)) }(breakOut))
  }

  implicit val moneyWriter: ToJSON[Money] = new ToJSON[Money] {
    def write(m: Money): JValue = JObject(
      JField("currencyCode", toJValue(m.currency)) ::
      JField("centAmount", toJValue(m.centAmount)) :: Nil
    )
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
    def write(u: Unit) = JNothing
  }

  implicit val dateTimeWriter: ToJSON[DateTime] = new ToJSON[DateTime] {
    def write(dt: DateTime): JValue = JString(ISODateTimeFormat.dateTime.print(dt.withZone(DateTimeZone.UTC)))
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