package io.sphere.json

import scalaz.{Failure, NonEmptyList, Success}
import scalaz.std.option._
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._
import scalaz.Validation.FlatMap._
import scala.util.control.NonFatal
import java.util.{Currency, Locale, UUID}

import io.sphere.util.{LangTag, Money}
import org.json4s.JsonAST._
import org.joda.time._
import org.joda.time.format.ISODateTimeFormat

/** Type class for types that can be read from JSON. */
trait FromJSON[A] {
  def read(jval: JValue): JValidation[A]
  final protected def fail(msg: String) = jsonParseError(msg)
  /** needed JSON fields - ignored if empty */
  def fields: Set[String] = Set.empty
}

object FromJSON {

  implicit def optionReader[A](implicit c: FromJSON[A]): FromJSON[Option[A]] = new FromJSON[Option[A]] {
    def read(jval: JValue): JValidation[Option[A]] = jval match {
      case JNothing | JNull | JObject(Nil) => Success(None)
      case JObject(s) if fields.nonEmpty && s.forall(t ⇒ !fields.contains(t._1)) ⇒ Success(None) // if none of the optional fields are in the JSON
      case x => c.read(x).map(some)
    }
    override val fields = c.fields
  }

  implicit def listReader[A](implicit r: FromJSON[A]): FromJSON[List[A]] = new FromJSON[List[A]] {
    import scala.collection.mutable.ListBuffer

    def read(jval: JValue): JValidation[List[A]] = jval match {
      case JArray(l) =>
        val initial: JValidation[ListBuffer[A]] = Success(new ListBuffer())
        l.foldLeft(initial) { (acc, jvalue) ⇒
          val result = r.read(jvalue)
          (acc, result) match {
            case (Success(builder), Success(e)) ⇒ Success(builder += e)
            case (Success(_), i @ Failure(_)) ⇒ i
            case (Failure(e1), Failure(e2)) ⇒ Failure(e1.append(e2))
            case (i @ Failure(_), _) ⇒ i
          }
        }.map(_.result())
      case _ => fail("JSON Array expected.")
    }
  }

  implicit def seqReader[A](implicit r: FromJSON[A]): FromJSON[Seq[A]] = new FromJSON[Seq[A]] {
    def read(jval: JValue): JValidation[Seq[A]] = jval match {
      case JArray(l) => l.traverse[JValidation, A](r.read)
      case _ => fail("JSON Array expected.")
    }
  }

  implicit def setReader[A](implicit r: FromJSON[A]): FromJSON[Set[A]] = new FromJSON[Set[A]] {
    def read(jval: JValue): JValidation[Set[A]] = jval match {
      case JArray(l) => l.traverse[JValidation, A](r.read).map(Set(_:_*))
      case _ => fail("JSON Array expected.")
    }
  }

  implicit def vectorReader[A](implicit r: FromJSON[A]): FromJSON[Vector[A]] = new FromJSON[Vector[A]] {
    import scala.collection.immutable.VectorBuilder

    def read(jval: JValue): JValidation[Vector[A]] = jval match {
      case JArray(l) =>
        val initial: JValidation[VectorBuilder[A]] = Success(new VectorBuilder())
        l.foldLeft(initial) { (acc, jvalue) ⇒
          val result = r.read(jvalue)
          (acc, result) match {
            case (Success(builder), Success(e)) ⇒ Success(builder += e)
            case (Success(_), i @ Failure(_)) ⇒ i
            case (Failure(e1), Failure(e2)) ⇒ Failure(e1.append(e2))
            case (i @ Failure(_), _) ⇒ i
          }
        }.map(_.result())
      case _ => fail("JSON Array expected.")
    }
  }

  implicit def nonEmptyListReader[A](implicit r: FromJSON[A]): FromJSON[NonEmptyList[A]] = new FromJSON[NonEmptyList[A]] {
    def read(jval: JValue): JValidation[NonEmptyList[A]] =
      fromJValue[List[A]](jval) flatMap {
        case head :: tail => Success(NonEmptyList(head, tail:_*))
        case Nil => fail("Non-empty JSON array expected")
      }
  }

  implicit val intReader: FromJSON[Int] = new FromJSON[Int] {
    def read(jval: JValue): JValidation[Int] = jval match {
      case JInt(i) if i.isValidInt => Success(i.toInt)
      case JLong(i) if i.isValidInt => Success(i.toInt)
      case _ => fail("JSON Number in the range of an Int expected.")
    }
  }

  implicit val stringReader: FromJSON[String] = new FromJSON[String] {
    def read(jval: JValue): JValidation[String] = jval match {
      case JString(s) => Success(s)
      case _ => fail("JSON String expected.")
    }
  }

  implicit val bigIntReader: FromJSON[BigInt] = new FromJSON[BigInt] {
    def read(jval: JValue): JValidation[BigInt] = jval match {
      case JInt(i) => Success(i)
      case JLong(l) ⇒ Success(l)
      case _ => fail("JSON Number in the range of a BigInt expected.")
    }
  }

  implicit val shortReader: FromJSON[Short] = new FromJSON[Short] {
    def read(jval: JValue): JValidation[Short] = jval match {
      case JInt(i) if i.isValidShort => Success(i.toShort)
      case JLong(i) if i.isValidShort => Success(i.toShort)
      case _ => fail("JSON Number in the range of a Short expected.")
    }
  }

  implicit val longReader: FromJSON[Long] = new FromJSON[Long] {
    def read(jval: JValue): JValidation[Long] = jval match {
      case JInt(i) => Success(i.toLong)
      case JLong(i) => Success(i)
      case _ => fail("JSON Number in the range of a Long expected.")
    }
  }

  implicit val floatReader: FromJSON[Float] = new FromJSON[Float] {
    def read(jval: JValue): JValidation[Float] = jval match {
      case JDouble(d) => Success(d.toFloat)
      case _ => fail("JSON Number in the range of a Float expected.")
    }
  }

  implicit val doubleReader: FromJSON[Double] = new FromJSON[Double] {
    def read(jval: JValue): JValidation[Double] = jval match {
      case JDouble(d) => Success(d)
      case JInt(i) => Success(i.toDouble)
      case JLong(i) => Success(i.toDouble)
      case _ => fail("JSON Number in the range of a Double expected.")
    }
  }

  implicit val booleanReader: FromJSON[Boolean] = new FromJSON[Boolean] {
    def read(jval: JValue): JValidation[Boolean] = jval match {
      case JBool(b) => Success(b)
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
    override val fields = Set("centAmount", "currencyCode")
    def read(jval: JValue): JValidation[Money] = jval match {
      case o: JObject =>
        (field[Long]("centAmount")(o) <*>
        (field[Currency]("currencyCode")(o).map(mkMoney)))
      case _ => fail("JSON object expected.")
    }

    def mkMoney(currency: Currency)(centAmount: Long): Money = {
      Money.make(
        centAmount / math.pow(10, currency.getDefaultFractionDigits),
        currency)
    }
  }

  implicit val currencyReader: FromJSON[Currency] = new FromJSON[Currency] {
    val failMsg = "ISO 4217 code JSON String expected"
    def read(jval: JValue): JValidation[Currency] = jval match {
      case JString(s) =>
        try {
          Success(Currency.getInstance(s))
        } catch {
          case e: IllegalArgumentException => fail(failMsg)
        }
      case _ => fail(failMsg)
    }
  }

  implicit val jValueReader: FromJSON[JValue] = new FromJSON[JValue] {
    def read(jval: JValue): JValidation[JValue] = Success(jval)
  }

  implicit val jObjectReader: FromJSON[JObject] = new FromJSON[JObject] {
    def read(jval: JValue): JValidation[JObject] = jval match {
      case o: JObject => Success(o)
      case _ => fail("JSON object expected")
    }
  }

  implicit val unitReader: FromJSON[Unit] = new FromJSON[Unit] {
    def read(jval: JValue): JValidation[Unit] = jval match {
      case JNothing | JNull | JObject(Nil) => Success(())
      case _ => fail("Unexpected JSON")
    }
  }

  implicit val dateTimeReader: FromJSON[DateTime] = new FromJSON[DateTime] {
    def read(jval: JValue): JValidation[DateTime] = jval match {
      case JString(s) => try {
        Success(new DateTime(s, DateTimeZone.UTC))
      } catch {
        case NonFatal(_) => fail("Failed to parse date/time: %s".format(s))
      }
      case _ => fail("JSON Object expected.")
    }
  }

  implicit val timeReader: FromJSON[LocalTime] = new FromJSON[LocalTime] {
    def read(jval: JValue): JValidation[LocalTime] = jval match {
      case JString(s) => try {
        Success(ISODateTimeFormat.localTimeParser.parseDateTime(s).toLocalTime)
      } catch {
        case NonFatal(_) => fail("Failed to parse time: %s".format(s))
      }
      case _ => fail("JSON string expected.")
    }
  }

  implicit val dateReader: FromJSON[LocalDate] = new FromJSON[LocalDate] {
    def read(jval: JValue): JValidation[LocalDate] = jval match {
      case JString(s) => try {
        Success(ISODateTimeFormat.localDateParser.parseDateTime(s).toLocalDate)
      } catch {
        case NonFatal(_) => fail("Failed to parse date: %s".format(s))
      }
      case _ => fail("JSON string expected.")
    }
  }

  implicit val yearMonthReader: FromJSON[YearMonth] = new FromJSON[YearMonth] {
    def read(jval: JValue): JValidation[YearMonth] = jval match {
      case JString(s) => try {
        Success(new YearMonth(s))
      } catch {
        case NonFatal(_) => fail("Failed to parse year/month: %s".format(s))
      }
      case _ => fail("JSON Object expected.")
    }
  }

  implicit val uuidReader: FromJSON[UUID] = new FromJSON[UUID] {
    def read(jval: JValue): JValidation[UUID] = jval match {
      case JString(s) => try {
        Success(UUID.fromString(s))
      } catch {
        case NonFatal(_) => fail("Invalid UUID: '%s'".format(s))
      }
      case _ => fail("JSON string expected.")
    }
  }

  implicit val localeReader: FromJSON[Locale] = new FromJSON[Locale] {
    def read(jval: JValue): JValidation[Locale] = jval match {
      case JString(s) => s match {
        case LangTag(langTag) => Success(langTag)
        case _ => fail(LangTag.invalidLangTagMessage(s))
      }
      case _ => fail("JSON string expected.")
    }
  }
}
