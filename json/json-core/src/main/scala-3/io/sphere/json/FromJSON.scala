package io.sphere.json

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.apply.*
import cats.syntax.traverse.*
import io.sphere.json.field
import io.sphere.json.generic.JSONTypeSwitch.FromFormatters
import io.sphere.util.*
import org.joda.time.*
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonAST.*

import java.time
import java.util.{Currency, Locale, UUID}
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/** Type class for types that can be read from JSON. */
trait FromJSON[A] extends Serializable {
  def read(jval: JValue): JValidation[A]
  final protected def fail(msg: String) = jsonParseError(msg)

  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = FromJSON.emptyFieldsSet

  // This is automatically filled for traits
  val fromFormatters: FromFormatters = null
  def getSerializedNames: Vector[String] =
    if (fromFormatters == null) Vector.empty
    else fromFormatters.serializedNames
}

object FromJSON extends FromJSONCatsInstances with generic.DeriveFromJSON {
  val emptyFieldsSet: Set[String] = Set.empty

  inline def apply[A](using instance: FromJSON[A]): FromJSON[A] = instance

  private val validNone = Valid(None)
  private val validNil = Valid(Nil)
  private val validEmptyAnyVector: Valid[Vector[Any]] = Valid(Vector.empty)
  private def validList[A]: Valid[List[A]] = validNil
  private def validEmptyVector[A]: Valid[Vector[A]] =
    validEmptyAnyVector.asInstanceOf[Valid[Vector[A]]]

  def instance[A](
      readFn: JValue => JValidation[A],
      fromFs: FromFormatters,
      fieldSet: Set[String] = emptyFieldsSet): FromJSON[A] = new {

    override def read(jval: JValue): JValidation[A] = readFn(jval)
    override val fields: Set[String] = fieldSet
    override val fromFormatters: FromFormatters = fromFs
  }

  given optionMapReader[A](using c: FromJSON[A]): FromJSON[Option[Map[String, A]]] with {
    private val internalMapReader = mapReader[A]

    def read(jval: JValue): JValidation[Option[Map[String, A]]] = jval match {
      case JNothing | JNull => validNone
      case x => internalMapReader.read(x).map(Some.apply)
    }
  }

  given optionReader[A](using c: FromJSON[A]): FromJSON[Option[A]] with {
    def read(jval: JValue): JValidation[Option[A]] = jval match {
      case JNothing | JNull | JObject(Nil) => validNone
      case JObject(s) if fields.nonEmpty && s.forall(t => !fields.contains(t._1)) =>
        validNone // if none of the optional fields are in the JSON
      case x => c.read(x).map(Option.apply)
    }

    override val fields: Set[String] = c.fields
  }

  given listReader[A](using r: FromJSON[A]): FromJSON[List[A]] with {
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

  given seqReader[A](using r: FromJSON[A]): FromJSON[Seq[A]] with {
    def read(jval: JValue): JValidation[Seq[A]] = summon[FromJSON[List[A]]].read(jval)
  }

  given setReader[A](using r: FromJSON[A]): FromJSON[Set[A]] with {
    def read(jval: JValue): JValidation[Set[A]] = jval match {
      case JArray(l) =>
        if (l.isEmpty) Valid(Set.empty)
        else summon[FromJSON[List[A]]].read(jval).map(Set.apply(_: _*))
      case _ => fail("JSON Array expected.")
    }
  }

  given vectorReader[A](using r: FromJSON[A]): FromJSON[Vector[A]] with {
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

  given nonEmptyListReader[A](using r: FromJSON[A]): FromJSON[NonEmptyList[A]] with {
    def read(jval: JValue): JValidation[NonEmptyList[A]] =
      fromJValue[List[A]](jval).andThen {
        case head :: tail => Valid(NonEmptyList(head, tail))
        case Nil => fail("Non-empty JSON array expected")
      }
  }

  given intReader: FromJSON[Int] with {
    def read(jval: JValue): JValidation[Int] = jval match {
      case JInt(i) if i.isValidInt => Valid(i.toInt)
      case JLong(i) if i.isValidInt => Valid(i.toInt)
      case _ => fail("JSON Number in the range of an Int expected.")
    }
  }

  given stringReader: FromJSON[String] with {
    def read(jval: JValue): JValidation[String] = jval match {
      case JString(s) => Valid(s)
      case _ => fail("JSON String expected.")
    }
  }

  given bigIntReader: FromJSON[BigInt] with {
    def read(jval: JValue): JValidation[BigInt] = jval match {
      case JInt(i) => Valid(i)
      case JLong(l) => Valid(l)
      case _ => fail("JSON Number in the range of a BigInt expected.")
    }
  }

  given shortReader: FromJSON[Short] with {
    def read(jval: JValue): JValidation[Short] = jval match {
      case JInt(i) if i.isValidShort => Valid(i.toShort)
      case JLong(l) if l.isValidShort => Valid(l.toShort)
      case _ => fail("JSON Number in the range of a Short expected.")
    }
  }

  given longReader: FromJSON[Long] with {
    def read(jval: JValue): JValidation[Long] = jval match {
      case JInt(i) => Valid(i.toLong)
      case JLong(l) => Valid(l)
      case _ => fail("JSON Number in the range of a Long expected.")
    }
  }

  given floatReader: FromJSON[Float] with {
    def read(jval: JValue): JValidation[Float] = jval match {
      case JDouble(d) => Valid(d.toFloat)
      case _ => fail("JSON Number in the range of a Float expected.")
    }
  }

  given doubleReader: FromJSON[Double] with {
    def read(jval: JValue): JValidation[Double] = jval match {
      case JDouble(d) => Valid(d)
      case JInt(i) => Valid(i.toDouble)
      case JLong(l) => Valid(l.toDouble)
      case _ => fail("JSON Number in the range of a Double expected.")
    }
  }

  given booleanReader: FromJSON[Boolean] with {
    private val cachedTrue = Valid(true)
    private val cachedFalse = Valid(false)

    def read(jval: JValue): JValidation[Boolean] = jval match {
      case JBool(b) => if (b) cachedTrue else cachedFalse
      case _ => fail("JSON Boolean expected")
    }
  }

  given mapReader[A: FromJSON]: FromJSON[Map[String, A]] with {
    def read(json: JValue): JValidation[Map[String, A]] = json match {
      case JObject(fs) =>
        // Perf note: an imperative implementation does not seem faster
        fs.traverse[JValidation, (String, A)] { f =>
          fromJValue[A](f._2).map(v => (f._1, v))
        }.map(_.toMap)
      case _ => fail("JSON Object expected")
    }
  }

  given moneyReader: FromJSON[Money] with {
    import Money.*

    override val fields = Set(CentAmountField, CurrencyCodeField)

    def read(value: JValue): JValidation[Money] = value match {
      case o: JObject =>
        (field[Long](CentAmountField)(o), field[Currency](CurrencyCodeField)(o)) match {
          case (Valid(centAmount), Valid(currencyCode)) =>
            Valid(Money.fromCentAmount(centAmount, currencyCode))
          case (Invalid(e1), Invalid(e2)) => Invalid(e1.concat(e2.toList))
          case (e1 @ Invalid(_), _) => e1
          case (_, e2 @ Invalid(_)) => e2
        }

      case _ => fail("JSON object expected.")
    }
  }

  given highPrecisionMoneyReader: FromJSON[HighPrecisionMoney] with {
    import HighPrecisionMoney.*

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
              .leftMap(_.map(JSONParseError.apply))
        }

      case _ =>
        fail("JSON object expected.")
    }
  }

  given baseMoneyReader: FromJSON[BaseMoney] with {
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

  given currencyReader: FromJSON[Currency] with {
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

  given jValueReader: FromJSON[JValue] with {
    def read(jval: JValue): JValidation[JValue] = Valid(jval)
  }

  given jObjectReader: FromJSON[JObject] with {
    def read(jval: JValue): JValidation[JObject] = jval match {
      case o: JObject => Valid(o)
      case _ => fail("JSON object expected")
    }
  }

  private val validUnit = Valid(())

  given unitReader: FromJSON[Unit] with {
    def read(jval: JValue): JValidation[Unit] = jval match {
      case JNothing | JNull | JObject(Nil) => validUnit
      case _ => fail("Unexpected JSON")
    }
  }

  private def jsonStringReader[T](errorMessageTemplate: String)(
      fromString: String => T): FromJSON[T] =
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

  // Joda Time
  given dateTimeReader: FromJSON[DateTime] =
    jsonStringReader("Failed to parse date/time: %s")(parseJodaTime)

  private final val UTCDateTimeComponents =
    raw"(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})Z".r

  private def parseJodaTime(s: String): DateTime = s match {
    case UTCDateTimeComponents(year, month, days, hours, minutes, seconds, millis) =>
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

  given timeReader: FromJSON[LocalTime] = jsonStringReader("Failed to parse time: %s") {
    ISODateTimeFormat.localTimeParser.parseDateTime(_).toLocalTime
  }

  given dateReader: FromJSON[LocalDate] = jsonStringReader("Failed to parse date: %s") {
    ISODateTimeFormat.localDateParser.parseDateTime(_).toLocalDate
  }

  given yearMonthReader: FromJSON[YearMonth] =
    jsonStringReader("Failed to parse year/month: %s") {
      new YearMonth(_)
    }

  given javaInstantReader: FromJSON[time.Instant] =
    jsonStringReader("Failed to parse date/time: %s") { s =>
      try DateTimeFormats.parseInstantUnsafe(s)
      catch {
        case NonFatal(e) =>
          val result = time.Instant.ofEpochMilli(parseJodaTime(s).getMillis)
          // only log if the joda parsing does not fail
          log.error(
            s"Failed to parse date/time '$s' with java.time.Instant, falling back to Joda time.",
            e)
          result
      }
    }

  given javaLocalTimeReader: FromJSON[time.LocalTime] =
    jsonStringReader("Failed to parse time: %s")(DateTimeFormats.parseLocalTimeUnsafe)

  given javaLocalDateReader: FromJSON[time.LocalDate] =
    jsonStringReader("Failed to parse date: %s")(DateTimeFormats.parseLocalDateUnsafe)

  given javaYearMonthReader: FromJSON[time.YearMonth] =
    jsonStringReader("Failed to parse year/month: %s")(
      time.YearMonth.parse(_, JavaYearMonthFormatter))

  given uuidReader: FromJSON[UUID] = jsonStringReader("Invalid UUID: '%s'")(UUID.fromString)

  given localeReader: FromJSON[Locale] with {
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
