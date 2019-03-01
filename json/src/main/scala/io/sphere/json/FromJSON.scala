package io.sphere.json

import scala.util.control.NonFatal
import java.util.{Currency, Locale, UUID}

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.traverse._
import io.sphere.util.{BaseMoney, HighPrecisionMoney, LangTag, Money}
import org.json4s.JsonAST._
import org.joda.time._
import org.joda.time.format.ISODateTimeFormat

/** Type class for types that can be read from JSON. */
trait FromJSON[@specialized A] {
  def read(jval: JValue): JValidation[A]
  final protected def fail(msg: String) = jsonParseError(msg)
  /** needed JSON fields - ignored if empty */
  def fields: Set[String] = Set.empty
}

object FromJSON {

  @inline def apply[A](implicit instance: FromJSON[A]): FromJSON[A] = instance

  private val validNone = Valid(None)

  implicit def optionReader[@specialized A](implicit c: FromJSON[A]): FromJSON[Option[A]] = new FromJSON[Option[A]] {
    def read(jval: JValue): JValidation[Option[A]] = jval match {
      case JNothing | JNull | JObject(Nil) => validNone
      case JObject(s) if fields.nonEmpty && s.forall(t ⇒ !fields.contains(t._1)) ⇒ validNone // if none of the optional fields are in the JSON
      case x => c.read(x).map(Option.apply)
    }
    override val fields = c.fields
  }

  implicit def listReader[@specialized A](implicit r: FromJSON[A]): FromJSON[List[A]] = new FromJSON[List[A]] {
    import scala.collection.mutable.ListBuffer

    def read(jval: JValue): JValidation[List[A]] = jval match {
      case JArray(l) =>
        // "imperative" style for performances
        val initial: JValidation[ListBuffer[A]] = Valid(new ListBuffer())
        l.foldLeft(initial) { (acc, jvalue) ⇒
          val result = r.read(jvalue)
          (acc, result) match {
            case (Valid(builder), Valid(e)) ⇒ Valid(builder += e)
            case (Valid(_), i @ Invalid(_)) ⇒ i
            case (Invalid(e1), Invalid(e2)) ⇒ Invalid(e1.concatNel(e2))
            case (i @ Invalid(_), _) ⇒ i
          }
        }.map(_.result())
      case _ => fail("JSON Array expected.")
    }
  }

  implicit def seqReader[@specialized A](implicit r: FromJSON[A]): FromJSON[Seq[A]] = new FromJSON[Seq[A]] {
    def read(jval: JValue): JValidation[Seq[A]] = listReader(r).read(jval)
  }

  implicit def setReader[@specialized A](implicit r: FromJSON[A]): FromJSON[Set[A]] = new FromJSON[Set[A]] {
    def read(jval: JValue): JValidation[Set[A]] = jval match {
      case JArray(l) => l.traverse[JValidation, A](r.read).map(Set(_:_*))
      case _ => fail("JSON Array expected.")
    }
  }

  implicit def vectorReader[@specialized A](implicit r: FromJSON[A]): FromJSON[Vector[A]] = new FromJSON[Vector[A]] {
    import scala.collection.immutable.VectorBuilder

    def read(jval: JValue): JValidation[Vector[A]] = jval match {
      case JArray(l) =>
        // "imperative" style for performances
        val initial: JValidation[VectorBuilder[A]] = Valid(new VectorBuilder())
        l.foldLeft(initial) { (acc, jvalue) ⇒
          val result = r.read(jvalue)
          (acc, result) match {
            case (Valid(builder), Valid(e)) ⇒ Valid(builder += e)
            case (Valid(_), i @ Invalid(_)) ⇒ i
            case (Invalid(e1), Invalid(e2)) ⇒ Invalid(e1.concatNel(e2))
            case (i @ Invalid(_), _) ⇒ i
          }
        }.map(_.result())
      case _ => fail("JSON Array expected.")
    }
  }

  implicit def nonEmptyListReader[A](implicit r: FromJSON[A]): FromJSON[NonEmptyList[A]] = new FromJSON[NonEmptyList[A]] {
    def read(jval: JValue): JValidation[NonEmptyList[A]] =
      fromJValue[List[A]](jval) andThen {
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
      case JLong(l) ⇒ Valid(l)
      case _ => fail("JSON Number in the range of a BigInt expected.")
    }
  }

  implicit val shortReader: FromJSON[Short] = new FromJSON[Short] {
    def read(jval: JValue): JValidation[Short] = jval match {
      case JInt(i) if i.isValidShort => Valid(i.toShort)
      case JLong(i) if i.isValidShort => Valid(i.toShort)
      case _ => fail("JSON Number in the range of a Short expected.")
    }
  }

  implicit val longReader: FromJSON[Long] = new FromJSON[Long] {
    def read(jval: JValue): JValidation[Long] = jval match {
      case JInt(i) => Valid(i.toLong)
      case JLong(i) => Valid(i)
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
      case JLong(i) => Valid(i.toDouble)
      case _ => fail("JSON Number in the range of a Double expected.")
    }
  }

  implicit val booleanReader: FromJSON[Boolean] = new FromJSON[Boolean] {
    def read(jval: JValue): JValidation[Boolean] = jval match {
      case JBool(b) => Valid(b)
      case _ => fail("JSON Boolean expected")
    }
  }

  implicit def mapReader[A: FromJSON]: FromJSON[Map[String, A]] = new FromJSON[Map[String, A]] {
    def read(json: JValue) = json match {
      case JObject(fs) =>
        fs.traverse[JValidation, (String, A)] { f =>
          fromJValue[A](f._2).map(v => (f._1, v))
        }.map(_.toMap)
      case x => fail("JSON Object expected")
    }
  }

  implicit val moneyReader: FromJSON[Money] = new FromJSON[Money] {
    import Money._

    override val fields = Set(CentAmountField, CurrencyCodeField)

    def read(value: JValue): JValidation[Money] = value match {
      case o: JObject ⇒
        (field[Long](CentAmountField)(o), field[Currency](CurrencyCodeField)(o)).mapN(
          Money.fromCentAmount)

      case _ ⇒ fail("JSON object expected.")
    }
  }

  implicit val highPrecisionMoneyReader: FromJSON[HighPrecisionMoney] = new FromJSON[HighPrecisionMoney] {
    import HighPrecisionMoney._

    override val fields = Set(PreciseAmountField, CurrencyCodeField, FractionDigitsField)

    import cats.implicits._
    
    def read(value: JValue): JValidation[HighPrecisionMoney] = value match {
      case o: JObject ⇒
        val validatedFields = (
          field[Long](PreciseAmountField)(o),
          field[Int](FractionDigitsField)(o),
          field[Currency](CurrencyCodeField)(o),
          field[Option[Long]](CentAmountField)(o))

        validatedFields.tupled.andThen { case (preciseAmount, fractionDigits, currencyCode, centAmount) ⇒
          HighPrecisionMoney.fromPreciseAmount(preciseAmount, fractionDigits, currencyCode, centAmount).leftMap(_.map(JSONParseError(_)))
        }

      case _ ⇒
        fail("JSON object expected.")
    }
  }

  implicit val baseMoneyReader: FromJSON[BaseMoney] = new FromJSON[BaseMoney] {
    def read(value: JValue): JValidation[BaseMoney] = value match {
      case o: JObject ⇒
        field[Option[String]](BaseMoney.TypeField)(o).andThen {
          case None ⇒ moneyReader.read(value)
          case Some(Money.TypeName) ⇒ moneyReader.read(value)
          case Some(HighPrecisionMoney.TypeName) ⇒ highPrecisionMoneyReader.read(value)
          case Some(tpe) ⇒ fail(s"Unknown money type '$tpe'. Available types are: '${Money.TypeName}', '${HighPrecisionMoney.TypeName}'.")
        }

      case _ ⇒ fail("JSON object expected.")
    }
  }

  implicit val currencyReader: FromJSON[Currency] = new FromJSON[Currency] {
    val failMsg = "ISO 4217 code JSON String expected."
    def failMsgFor(input: String) = s"Currency '$input' not valid as ISO 4217 code."

    def read(jval: JValue): JValidation[Currency] = jval match {
      case JString(s) =>
        try {
          Valid(Currency.getInstance(s))
        } catch {
          case e: IllegalArgumentException => fail(failMsgFor(s))
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

  implicit val dateTimeReader: FromJSON[DateTime] = new FromJSON[DateTime] {
    def read(jval: JValue): JValidation[DateTime] = jval match {
      case JString(s) => try {
        Valid(new DateTime(s, DateTimeZone.UTC))
      } catch {
        case NonFatal(_) => fail("Failed to parse date/time: %s".format(s))
      }
      case _ => fail("JSON string expected.")
    }
  }

  implicit val timeReader: FromJSON[LocalTime] = new FromJSON[LocalTime] {
    def read(jval: JValue): JValidation[LocalTime] = jval match {
      case JString(s) => try {
        Valid(ISODateTimeFormat.localTimeParser.parseDateTime(s).toLocalTime)
      } catch {
        case NonFatal(_) => fail("Failed to parse time: %s".format(s))
      }
      case _ => fail("JSON string expected.")
    }
  }

  implicit val dateReader: FromJSON[LocalDate] = new FromJSON[LocalDate] {
    def read(jval: JValue): JValidation[LocalDate] = jval match {
      case JString(s) => try {
        Valid(ISODateTimeFormat.localDateParser.parseDateTime(s).toLocalDate)
      } catch {
        case NonFatal(_) => fail("Failed to parse date: %s".format(s))
      }
      case _ => fail("JSON string expected.")
    }
  }

  implicit val yearMonthReader: FromJSON[YearMonth] = new FromJSON[YearMonth] {
    def read(jval: JValue): JValidation[YearMonth] = jval match {
      case JString(s) => try {
        Valid(new YearMonth(s))
      } catch {
        case NonFatal(_) => fail("Failed to parse year/month: %s".format(s))
      }
      case _ => fail("JSON Object expected.")
    }
  }

  implicit val uuidReader: FromJSON[UUID] = new FromJSON[UUID] {
    def read(jval: JValue): JValidation[UUID] = jval match {
      case JString(s) => try {
        Valid(UUID.fromString(s))
      } catch {
        case NonFatal(_) => fail("Invalid UUID: '%s'".format(s))
      }
      case _ => fail("JSON string expected.")
    }
  }

  implicit val localeReader: FromJSON[Locale] = new FromJSON[Locale] {
    def read(jval: JValue): JValidation[Locale] = jval match {
      case JString(s) => s match {
        case LangTag(langTag) => Valid(langTag)
        case _ => fail(LangTag.invalidLangTagMessage(s))
      }
      case _ => fail("JSON string expected.")
    }
  }
}
