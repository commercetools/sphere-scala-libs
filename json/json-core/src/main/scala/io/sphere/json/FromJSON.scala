package io.sphere.json

import scala.util.control.NonFatal
import scala.collection.mutable.ListBuffer
import java.util.{Currency, Locale, UUID}
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.apply._
import cats.syntax.traverse._
import io.sphere.json.field
import io.sphere.util.{BaseMoney, HighPrecisionMoney, LangTag, Money}
import org.json4s.JsonAST._
import org.joda.time._
import org.joda.time.format.ISODateTimeFormat

import scala.annotation.implicitNotFound

/** Type class for types that can be read from JSON. */
@implicitNotFound("Could not find an instance of FromJSON for ${A}")
trait FromJSON[@specialized A] extends Serializable {
  def read(jval: JValue): JValidation[A]

  final protected def fail(msg: String) = jsonParseError(msg)

  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = FromJSON.emptyFieldsSet
}

object FromJSON extends FromJSONInstances {

  private[FromJSON] val emptyFieldsSet: Set[String] = Set.empty

  @inline def apply[A](implicit instance: FromJSON[A]): FromJSON[A] = instance

  private val validNone = Valid(None)
  private val validNil = Valid(Nil)
  private val validEmptyAnyVector: Valid[Vector[Any]] = Valid(Vector.empty)

  private def validList[A]: Valid[List[A]] = validNil

  private def validEmptyVector[A]: Valid[Vector[A]] =
    validEmptyAnyVector.asInstanceOf[Valid[Vector[A]]]

  implicit def optionReader[@specialized A](implicit c: FromJSON[A]): FromJSON[Option[A]] =
    new FromJSON[Option[A]] {
      def read(jval: JValue): JValidation[Option[A]] = jval match {
        case JNothing | JNull | JObject(Nil) => validNone
        case JObject(s) if fields.nonEmpty && s.forall(t => !fields.contains(t._1)) =>
          validNone // if none of the optional fields are in the JSON
        case x => c.read(x).map(Option.apply)
      }

      override val fields: Set[String] = c.fields
    }

  implicit def listReader[@specialized A](implicit r: FromJSON[A]): FromJSON[List[A]] =
    new FromJSON[List[A]] {

      def read(jval: JValue): JValidation[List[A]] = jval match {
        case JArray(l) =>
          if (l.isEmpty) validList[A]
          else {
            // "imperative" style for performances
            val errors = new ListBuffer[JSONError]()
            val valids = new ListBuffer[A]()
            var failedOnce: Boolean = false
            l.foreach { jval =>
              r.read(jval) match {
                case Valid(s) if !failedOnce =>
                  valids += s
                case Invalid(nel) =>
                  errors ++= nel.toList
                  failedOnce = true
                case _ => ()
              }
            }
            if (errors.isEmpty)
              Valid(valids.result())
            else
              Invalid(NonEmptyList.fromListUnsafe(errors.result()))
          }
        case _ => fail("JSON Array expected.")
      }
    }

  implicit def seqReader[@specialized A](implicit r: FromJSON[A]): FromJSON[Seq[A]] =
    new FromJSON[Seq[A]] {
      def read(jval: JValue): JValidation[Seq[A]] = listReader(r).read(jval)
    }

  implicit def setReader[@specialized A](implicit r: FromJSON[A]): FromJSON[Set[A]] =
    new FromJSON[Set[A]] {
      def read(jval: JValue): JValidation[Set[A]] = jval match {
        case JArray(l) =>
          if (l.isEmpty) Valid(Set.empty)
          else listReader(r).read(jval).map(Set.apply(_: _*))
        case _ => fail("JSON Array expected.")
      }
    }

  implicit def vectorReader[@specialized A](implicit r: FromJSON[A]): FromJSON[Vector[A]] =
    new FromJSON[Vector[A]] {

      import scala.collection.immutable.VectorBuilder

      def read(jval: JValue): JValidation[Vector[A]] = jval match {
        case JArray(l) =>
          if (l.isEmpty) validEmptyVector
          else {
            // "imperative" style for performances
            val errors = new ListBuffer[JSONError]()
            val valids = new VectorBuilder[A]()
            var failedOnce: Boolean = false
            l.foreach { jval =>
              r.read(jval) match {
                case Valid(s) if !failedOnce =>
                  valids += s
                case Invalid(nel) =>
                  errors ++= nel.toList
                  failedOnce = true
                case _ => ()
              }
            }
            if (errors.isEmpty)
              Valid(valids.result())
            else
              Invalid(NonEmptyList.fromListUnsafe(errors.result()))
          }
        case _ => fail("JSON Array expected.")
      }
    }

  implicit def nonEmptyListReader[A](implicit r: FromJSON[A]): FromJSON[NonEmptyList[A]] =
    new FromJSON[NonEmptyList[A]] {
      def read(jval: JValue): JValidation[NonEmptyList[A]] =
        fromJValue[List[A]](jval).andThen {
          case head :: tail => Valid(NonEmptyList(head, tail))
          case Nil => fail("Non-empty JSON array expected")
        }
    }

  implicit val intReader: FromJSON[Int] = new FromJSON[Int] {
    def read(jval: JValue): JValidation[Int] = jval match {
      case JInt(i) if i.isValidInt => Valid(i.toInt)
      case JLong(i) if i.isValidInt => Valid(i.toInt)
      case _ => fail("JSON Number in the range of an Int expected.")
    }
  }

  implicit val stringReader: FromJSON[String] = new FromJSON[String] {
    def read(jval: JValue): JValidation[String] = jval match {
      case JString(s) => Valid(s)
      case _ => fail("JSON String expected.")
    }
  }

  implicit val bigIntReader: FromJSON[BigInt] = new FromJSON[BigInt] {
    def read(jval: JValue): JValidation[BigInt] = jval match {
      case JInt(i) => Valid(i)
      case JLong(l) => Valid(l)
      case _ => fail("JSON Number in the range of a BigInt expected.")
    }
  }

  implicit val shortReader: FromJSON[Short] = new FromJSON[Short] {
    def read(jval: JValue): JValidation[Short] = jval match {
      case JInt(i) if i.isValidShort => Valid(i.toShort)
      case JLong(l) if l.isValidShort => Valid(l.toShort)
      case _ => fail("JSON Number in the range of a Short expected.")
    }
  }

  implicit val longReader: FromJSON[Long] = new FromJSON[Long] {
    def read(jval: JValue): JValidation[Long] = jval match {
      case JInt(i) => Valid(i.toLong)
      case JLong(l) => Valid(l)
      case _ => fail("JSON Number in the range of a Long expected.")
    }
  }

  implicit val floatReader: FromJSON[Float] = new FromJSON[Float] {
    def read(jval: JValue): JValidation[Float] = jval match {
      case JDouble(d) => Valid(d.toFloat)
      case _ => fail("JSON Number in the range of a Float expected.")
    }
  }

  implicit val doubleReader: FromJSON[Double] = new FromJSON[Double] {
    def read(jval: JValue): JValidation[Double] = jval match {
      case JDouble(d) => Valid(d)
      case JInt(i) => Valid(i.toDouble)
      case JLong(l) => Valid(l.toDouble)
      case _ => fail("JSON Number in the range of a Double expected.")
    }
  }

  implicit val booleanReader: FromJSON[Boolean] = new FromJSON[Boolean] {
    private val cachedTrue = Valid(true)
    private val cachedFalse = Valid(false)

    def read(jval: JValue): JValidation[Boolean] = jval match {
      case JBool(b) => if (b) cachedTrue else cachedFalse
      case _ => fail("JSON Boolean expected")
    }
  }

  implicit def mapReader[A: FromJSON]: FromJSON[Map[String, A]] = new FromJSON[Map[String, A]] {
    def read(json: JValue): JValidation[Map[String, A]] = json match {
      case JObject(fs) =>
        // Perf note: an imperative implementation does not seem faster
        fs.traverse[JValidation, (String, A)] { f =>
          fromJValue[A](f._2).map(v => (f._1, v))
        }.map(_.toMap)
      case _ => fail("JSON Object expected")
    }
  }

  implicit val moneyReader: FromJSON[Money] = new FromJSON[Money] {

    import Money._

    override val fields = Set(CentAmountField, CurrencyCodeField)

    def read(value: JValue): JValidation[Money] = value match {
      case o: JObject =>
        (field[Long](CentAmountField)(o), field[Currency](CurrencyCodeField)(o)) match {
          case (Valid(centAmount), Valid(currencyCode)) =>
            Valid(Money.fromCentAmount(centAmount, currencyCode))
          case (Invalid(e1), Invalid(e2)) => Invalid(e1.concat(e2.toList))
          case (e1@Invalid(_), _) => e1
          case (_, e2@Invalid(_)) => e2
        }

      case _ => fail("JSON object expected.")
    }
  }

  implicit val highPrecisionMoneyReader: FromJSON[HighPrecisionMoney] =
    new FromJSON[HighPrecisionMoney] {

      import HighPrecisionMoney._

      override val fields = Set(PreciseAmountField, CurrencyCodeField, FractionDigitsField)

      def read(value: JValue): JValidation[HighPrecisionMoney] = value match {
        case o: JObject =>
          val validatedFields = (
            field[Long](PreciseAmountField)(o),
            field[Int](FractionDigitsField)(o),
            field[Currency](CurrencyCodeField)(o),
            field[Option[Long]](CentAmountField)(o))

          validatedFields.tupled.andThen {
            case (preciseAmount, fractionDigits, currencyCode, centAmount) =>
              HighPrecisionMoney
                .fromPreciseAmount(preciseAmount, fractionDigits, currencyCode, centAmount)
                .leftMap(_.map(JSONParseError))
          }

        case _ =>
          fail("JSON object expected.")
      }
    }

  implicit val baseMoneyReader: FromJSON[BaseMoney] = new FromJSON[BaseMoney] {
    def read(value: JValue): JValidation[BaseMoney] = value match {
      case o: JObject =>
        field[Option[String]](BaseMoney.TypeField)(o).andThen {
          case None => moneyReader.read(value)
          case Some(Money.TypeName) => moneyReader.read(value)
          case Some(HighPrecisionMoney.TypeName) => highPrecisionMoneyReader.read(value)
          case Some(tpe) =>
            fail(
              s"Unknown money type '$tpe'. Available types are: '${Money.TypeName}', '${HighPrecisionMoney.TypeName}'.")
        }

      case _ => fail("JSON object expected.")
    }
  }

  implicit val currencyReader: FromJSON[Currency] = new FromJSON[Currency] {
    val failMsg = "ISO 4217 code JSON String expected."

    def failMsgFor(input: String) = s"Currency '$input' not valid as ISO 4217 code."

    private val cachedEUR = Valid(Currency.getInstance("EUR"))
    private val cachedUSD = Valid(Currency.getInstance("USD"))
    private val cachedGBP = Valid(Currency.getInstance("GBP"))
    private val cachedJPY = Valid(Currency.getInstance("JPY"))

    def read(jval: JValue): JValidation[Currency] = jval match {
      case JString(s) =>
        s match {
          case "EUR" => cachedEUR
          case "USD" => cachedUSD
          case "GBP" => cachedGBP
          case "JPY" => cachedJPY
          case _ =>
            try Valid(Currency.getInstance(s))
            catch {
              case _: IllegalArgumentException => fail(failMsgFor(s))
            }
        }
      case _ => fail(failMsg)
    }
  }

  implicit val jValueReader: FromJSON[JValue] = new FromJSON[JValue] {
    def read(jval: JValue): JValidation[JValue] = Valid(jval)
  }

  implicit val jObjectReader: FromJSON[JObject] = new FromJSON[JObject] {
    def read(jval: JValue): JValidation[JObject] = jval match {
      case o: JObject => Valid(o)
      case _ => fail("JSON object expected")
    }
  }

  private val validUnit = Valid(())

  implicit val unitReader: FromJSON[Unit] = new FromJSON[Unit] {
    def read(jval: JValue): JValidation[Unit] = jval match {
      case JNothing | JNull | JObject(Nil) => validUnit
      case _ => fail("Unexpected JSON")
    }
  }

  private def fromJsonString[T](errorMessageTemplate: String)(fromString: String => T): FromJSON[T] =
    new FromJSON[T] {
      def read(jval: JValue): JValidation[T] = jval match {
        case JString(s) =>
          try Valid(fromString(s))
          catch {
            case NonFatal(_) => fail(errorMessageTemplate.format(s))
          }
        case _ => fail("JSON string expected.")
      }
    }

  implicit val dateTimeReader: FromJSON[DateTime] = {
    val DateTimeParts = raw"(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})Z".r

    fromJsonString("Failed to parse date/time: %s") {
      case DateTimeParts(year, month, days, hours, minutes, seconds, millis) =>
        new DateTime(
          year.toInt,
          month.toInt,
          days.toInt,
          hours.toInt,
          minutes.toInt,
          seconds.toInt,
          millis.toInt,
          DateTimeZone.UTC)
      case otherwise =>
        new DateTime(otherwise, DateTimeZone.UTC)
    }
  }

  implicit val timeReader: FromJSON[LocalTime] = fromJsonString("Failed to parse time: %s") {
    ISODateTimeFormat.localTimeParser.parseDateTime(_).toLocalTime
  }

  implicit val dateReader: FromJSON[LocalDate] = fromJsonString("Failed to parse date: %s") {
    ISODateTimeFormat.localDateParser.parseDateTime(_).toLocalDate
  }

  implicit val yearMonthReader: FromJSON[YearMonth] = fromJsonString("Failed to parse year/month: %s") {
    new YearMonth(_)
  }

  implicit val uuidReader: FromJSON[UUID] = fromJsonString("Invalid UUID: '%s'")(UUID.fromString)

  implicit val localeReader: FromJSON[Locale] = new FromJSON[Locale] {
    def read(jval: JValue): JValidation[Locale] = jval match {
      case JString(s) =>
        s match {
          case LangTag(langTag) => Valid(langTag)
          case _ => fail(LangTag.invalidLangTagMessage(s))
        }
      case _ => fail("JSON string expected.")
    }
  }
}
