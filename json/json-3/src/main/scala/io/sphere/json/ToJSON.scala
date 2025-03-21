package io.sphere.json

import cats.data.NonEmptyList
import io.sphere.json.generic.{AnnotationReader, CaseClassMetaData, Field, TraitMetaData}
import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import org.joda.time.*
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonAST.*

import java.time
import java.util.{Currency, Locale, UUID}
import scala.deriving.Mirror

/** Type class for types that can be written to JSON. */
trait ToJSON[A] extends Serializable {
  def write(value: A): JValue
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONInstances {

  inline def apply[A: JSON]: ToJSON[A] = summon[ToJSON[A]]

  inline given derived[A](using Mirror.Of[A]): ToJSON[A] = Derivation.derived[A]

  private def addField(jObject: JObject, field: Field, jValue: JValue): JValue =
    jValue match {
      case o: JObject =>
        if (field.embedded) JObject(jObject.obj ++ o.obj)
        else JObject(jObject.obj :+ (field.fieldName -> o))
      case other => JObject(jObject.obj :+ (field.fieldName -> other))
    }

  private object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): ToJSON[A] =
      inline m match {
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): ToJSON[A] =
      new ToJSON[A] {
        private val traitMetaData: TraitMetaData = AnnotationReader.readTraitMetaData[A]
        private val typeHintMap: Map[String, String] = traitMetaData.subtypes.collect {
          case (name, classMeta) if classMeta.typeHint.isDefined =>
            name -> classMeta.typeHint.get
        }
        private val reverseTypeHintMap: Map[String, String] = typeHintMap.map((on, n) => (n, on))
        private val jsons: Seq[ToJSON[Any]] = summonToJson[mirrorOfSum.MirroredElemTypes]
        private val names: Seq[String] =
          constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
            .asInstanceOf[Vector[String]]
        private val jsonsByNames: Map[String, ToJSON[Any]] = names.zip(jsons).toMap

        override def write(value: A): JValue = {
          // we never get a trait here, only classes, it's safe to assume Product
          val originalTypeName = value.asInstanceOf[Product].productPrefix
          val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
          val json = jsonsByNames(originalTypeName).write(value).asInstanceOf[JObject]
          val typeDiscriminator = traitMetaData.typeDiscriminator -> JString(typeName)
          JObject(typeDiscriminator :: json.obj)
        }

      }

    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): ToJSON[A] =
      new ToJSON[A] {
        private val caseClassMetaData: CaseClassMetaData = AnnotationReader.readCaseClassMetaData[A]
        private val toJsons: Vector[ToJSON[Any]] = summonToJson[mirrorOfProduct.MirroredElemTypes]

        override def write(value: A): JValue = {
          val caseClassFields = value.asInstanceOf[Product].productIterator
          toJsons
            .zip(caseClassFields)
            .zip(caseClassMetaData.fields)
            .foldLeft[JValue](JObject()) { case (jObject, ((toJson, fieldValue), field)) =>
              addField(jObject.asInstanceOf[JObject], field, toJson.write(fieldValue))
            }
        }
      }

    inline private def summonToJson[T <: Tuple]: Vector[ToJSON[Any]] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[ToJSON[t]]
            .asInstanceOf[ToJSON[Any]] +: summonToJson[ts]
      }
  }

  private val emptyJArray = JArray(Nil)
  private val emptyJObject = JObject(Nil)

  inline def apply[A](implicit instance: ToJSON[A]): ToJSON[A] = instance

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)
  }

  given optionWriter[A](using c: ToJSON[A]): ToJSON[Option[A]] =
    new ToJSON[Option[A]] {
      def write(opt: Option[A]): JValue = opt match {
        case Some(a) => c.write(a)
        case None => JNothing
      }
    }

  implicit def listWriter[A](implicit w: ToJSON[A]): ToJSON[List[A]] =
    new ToJSON[List[A]] {
      def write(l: List[A]): JValue =
        if (l.isEmpty) emptyJArray
        else JArray(l.map(w.write))
    }

  implicit def nonEmptyListWriter[A](implicit w: ToJSON[A]): ToJSON[NonEmptyList[A]] =
    new ToJSON[NonEmptyList[A]] {
      def write(l: NonEmptyList[A]): JValue = JArray(l.toList.map(w.write))
    }

  implicit def seqWriter[A](implicit w: ToJSON[A]): ToJSON[Seq[A]] =
    new ToJSON[Seq[A]] {
      def write(s: Seq[A]): JValue =
        if (s.isEmpty) emptyJArray
        else JArray(s.iterator.map(w.write).toList)
    }

  implicit def setWriter[A](implicit w: ToJSON[A]): ToJSON[Set[A]] =
    new ToJSON[Set[A]] {
      def write(s: Set[A]): JValue =
        if (s.isEmpty) emptyJArray
        else JArray(s.iterator.map(w.write).toList)
    }

  implicit def vectorWriter[A](implicit w: ToJSON[A]): ToJSON[Vector[A]] =
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
    import Money.*

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

  // Joda time
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

  // java.time

  // always format the milliseconds
  private val javaInstantFormatter = new time.format.DateTimeFormatterBuilder()
    .appendInstant(3)
    .toFormatter()
  implicit val javaInstantWriter: ToJSON[time.Instant] = new ToJSON[time.Instant] {
    def write(value: time.Instant): JValue = JString(
      javaInstantFormatter.format(time.OffsetDateTime.ofInstant(value, time.ZoneOffset.UTC)))
  }

  // always format the milliseconds
  private val javaLocalTimeFormatter = new time.format.DateTimeFormatterBuilder()
    .appendPattern("HH:mm:ss.SSS")
    .toFormatter()
  implicit val javaTimeWriter: ToJSON[time.LocalTime] = new ToJSON[time.LocalTime] {
    def write(value: time.LocalTime): JValue = JString(javaLocalTimeFormatter.format(value))
  }

  implicit val javaDateWriter: ToJSON[time.LocalDate] = new ToJSON[time.LocalDate] {
    def write(value: time.LocalDate): JValue = JString(
      time.format.DateTimeFormatter.ISO_LOCAL_DATE.format(value))
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
