package io.sphere.mongo.format

import com.mongodb.BasicDBObject
import io.sphere.mongo.format
import io.sphere.mongo.format.SimpleMongoType
import io.sphere.util.{BaseMoney, HighPrecisionMoney, LangTag, Money}
import org.bson.{BSONObject, BasicBSONObject}
import org.bson.types.{BasicBSONList, ObjectId}

import java.util.{Currency, Locale, UUID}
import java.util.regex.Pattern
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ListBuffer

object DefaultMongoFormats extends DefaultMongoFormats {}

trait DefaultMongoFormats {
  given uuidFormat: MongoFormat[UUID] = new NativeMongoFormat[UUID]
  given objectIdFormat: MongoFormat[ObjectId] = new NativeMongoFormat[ObjectId]
  given stringFormat: MongoFormat[String] = new NativeMongoFormat[String]
  given shortFormat: MongoFormat[Short] = new NativeMongoFormat[Short]
  given intFormat: MongoFormat[Int] = new NativeMongoFormat[Int]
  given longFormat: MongoFormat[Long] = new MongoFormat[Long] {
    private val native = new NativeMongoFormat[Long]

    override def toMongoValue(a: Long): Any = native.toMongoValue(a)

    override def fromMongoValue(any: Any): Long =
      any match {
        // a Long can read from an Int (for example, old aggregates version)
        case i: Int => intFormat.fromMongoValue(i)
        case _ => native.fromMongoValue(any)
      }
  }
  given floatFormat: MongoFormat[Float] = new NativeMongoFormat[Float]
  given doubleFormat: MongoFormat[Double] = new NativeMongoFormat[Double]
  given booleanFormat: MongoFormat[Boolean] = new NativeMongoFormat[Boolean]
  given patternFormat: MongoFormat[Pattern] = new NativeMongoFormat[Pattern]

  given optionFormat[A](using format: MongoFormat[A]): MongoFormat[Option[A]] =
    new MongoFormat[Option[A]] {
      override def toMongoValue(a: Option[A]): Any =
        a match {
          case Some(value) => format.toMongoValue(value)
          case None => MongoNothing
        }

      override def fromMongoValue(mongoType: Any): Option[A] = {
        import scala.jdk.CollectionConverters.*
        val fieldNames = format.fields
        if (mongoType == null) None
        else
          mongoType match {
            case s: SimpleMongoType => Some(format.fromMongoValue(s))
            case bson: BasicDBObject =>
              val bsonFieldNames = bson.keySet().asScala
              if (fieldNames.nonEmpty && bsonFieldNames.intersect(fieldNames).isEmpty) None
              else Some(format.fromMongoValue(bson))
            case MongoNothing => None // This can't happen, but it makes the compiler happy
          }
      }

      override def default: Option[Option[A]] = Some(None)
    }

  given vecFormat[@specialized A](using format: MongoFormat[A]): MongoFormat[Vector[A]] =
    new MongoFormat[Vector[A]] {
      import scala.collection.JavaConverters._
      override def toMongoValue(a: Vector[A]) = {
        val m = new BasicBSONList()
        if (a.nonEmpty)
          m.addAll(a.map(format.toMongoValue(_).asInstanceOf[AnyRef]).asJavaCollection)
        m
      }
      override def fromMongoValue(any: Any): Vector[A] =
        any match {
          case l: BasicBSONList =>
            if (l.isEmpty) Vector.empty
            else {
              val builder = new VectorBuilder[A]
              val iter = l.iterator()
              while (iter.hasNext) {
                val element = iter.next()
                builder += format.fromMongoValue(element)
              }
              builder.result()
            }
          case _ => throw new Exception(s"cannot read value from ${any.getClass.getName}")
        }
    }

  given listFormat[@specialized A](using format: MongoFormat[A]): MongoFormat[List[A]] =
    new MongoFormat[List[A]] {
      import scala.collection.JavaConverters._
      override def toMongoValue(a: List[A]) = {
        val m = new BasicBSONList()
        if (a.nonEmpty)
          m.addAll(a.map(format.toMongoValue(_).asInstanceOf[AnyRef]).asJavaCollection)
        m
      }
      override def fromMongoValue(any: Any): List[A] =
        any match {
          case l: BasicBSONList =>
            if (l.isEmpty) Nil
            else {
              val builder = new ListBuffer[A]
              val iter = l.iterator()
              while (iter.hasNext) {
                val element = iter.next()
                builder += format.fromMongoValue(element)
              }
              builder.result()
            }
          case _ => throw new Exception(s"cannot read value from ${any.getClass.getName}")
        }
    }

  given setFormat[@specialized A](using f: MongoFormat[A]): MongoFormat[Set[A]] =
    new MongoFormat[Set[A]] {
      import scala.collection.JavaConverters._
      override def toMongoValue(a: Set[A]) = {
        val m = new BasicBSONList()
        if (a.nonEmpty)
          m.addAll(a.map(f.toMongoValue(_).asInstanceOf[AnyRef]).asJavaCollection)
        m
      }
      override def fromMongoValue(any: Any): Set[A] =
        any match {
          case l: BasicBSONList =>
            if (l.isEmpty) Set.empty
            else l.iterator().asScala.map(f.fromMongoValue).toSet
          case _ => throw new Exception(s"cannot read value from ${any.getClass.getName}")
        }
    }

  given mapFormat[@specialized A](using f: MongoFormat[A]): MongoFormat[Map[String, A]] =
    new MongoFormat[Map[String, A]] {
      override def toMongoValue(map: Map[String, A]): Any =
        // Perf note: new BasicBSONObject(map.size) is much slower for some reason
        map.foldLeft(new BasicBSONObject()) { case (dbo, (k, v)) =>
          dbo.append(k, summon[MongoFormat[A]].toMongoValue(v))
        }

      override def fromMongoValue(any: Any): Map[String, A] = {
        import scala.language.existentials

        val map: java.util.Map[?, ?] = any match {
          case b: BasicBSONObject => b // avoid instantiating a new map
          case dbo: BSONObject => dbo.toMap
          case other => throw new Exception(s"cannot read value from ${other.getClass.getName}")
        }
        val builder = Map.newBuilder[String, A]
        val iter = map.entrySet().iterator()
        while (iter.hasNext) {
          val entry = iter.next()
          val k = entry.getKey.asInstanceOf[String]
          val v = summon[MongoFormat[A]].fromMongoValue(entry.getValue)
          builder += (k -> v)
        }
        builder.result()
      }
    }

  given currencyFormat: MongoFormat[Currency] = new MongoFormat[Currency] {
    val failMsg = "ISO 4217 code JSON String expected."
    def failMsgFor(input: String) = s"Currency '$input' not valid as ISO 4217 code."

    override def toMongoValue(c: Currency): Any = c.getCurrencyCode
    override def fromMongoValue(any: Any): Currency = any match {
      case s: String =>
        try Currency.getInstance(s)
        catch {
          case _: IllegalArgumentException => throw new Exception(failMsgFor(s))
        }
      case _ => throw new Exception(failMsg)
    }
  }

  given moneyFormat: MongoFormat[Money] = new MongoFormat[Money] {
    import Money._

    override val fields = Set(CentAmountField, CurrencyCodeField)

    override def toMongoValue(m: Money): Any =
      new BasicBSONObject()
        .append(BaseMoney.TypeField, m.`type`)
        .append(CurrencyCodeField, currencyFormat.toMongoValue(m.currency))
        .append(CentAmountField, longFormat.toMongoValue(m.centAmount))
        .append(FractionDigitsField, m.currency.getDefaultFractionDigits)

    override def fromMongoValue(any: Any): Money = any match {
      case dbo: BSONObject =>
        Money.fromCentAmount(
          field[Long](CentAmountField, dbo),
          field[Currency](CurrencyCodeField, dbo))
      case other => throw new Exception(s"db object expected but has '${other.getClass.getName}'")
    }
  }

  given highPrecisionMoneyFormat: MongoFormat[HighPrecisionMoney] =
    new MongoFormat[HighPrecisionMoney] {
      import HighPrecisionMoney._

      override val fields = Set(PreciseAmountField, CurrencyCodeField, FractionDigitsField)

      override def toMongoValue(m: HighPrecisionMoney): Any =
        new BasicBSONObject()
          .append(BaseMoney.TypeField, m.`type`)
          .append(CurrencyCodeField, currencyFormat.toMongoValue(m.currency))
          .append(CentAmountField, longFormat.toMongoValue(m.centAmount))
          .append(PreciseAmountField, longFormat.toMongoValue(m.preciseAmount))
          .append(FractionDigitsField, m.fractionDigits)
      override def fromMongoValue(any: Any): HighPrecisionMoney = any match {
        case dbo: BSONObject =>
          HighPrecisionMoney
            .fromPreciseAmount(
              field[Long](PreciseAmountField, dbo),
              field[Int](FractionDigitsField, dbo),
              field[Currency](CurrencyCodeField, dbo),
              field[Option[Long]](CentAmountField, dbo)
            )
            .fold(nel => throw new Exception(nel.toList.mkString(", ")), identity)

        case other => throw new Exception(s"db object expected but has '${other.getClass.getName}'")
      }
    }

  given baseMoneyFormat: MongoFormat[BaseMoney] = new MongoFormat[BaseMoney] {
    override def toMongoValue(a: BaseMoney): Any = a match {
      case m: Money => moneyFormat.toMongoValue(m)
      case m: HighPrecisionMoney => highPrecisionMoneyFormat.toMongoValue(m)
    }
    override def fromMongoValue(any: Any): BaseMoney = any match {
      case dbo: BSONObject =>
        val typeField = dbo.get(BaseMoney.TypeField)
        if (typeField == null)
          moneyFormat.fromMongoValue(any)
        else
          stringFormat.fromMongoValue(typeField) match {
            case Money.TypeName => moneyFormat.fromMongoValue(any)
            case HighPrecisionMoney.TypeName =>
              highPrecisionMoneyFormat.fromMongoValue(any)
            case tpe =>
              throw new Exception(
                s"Unknown money type '$tpe'. Available types are: '${Money.TypeName}', '${HighPrecisionMoney.TypeName}'.")
          }
      case other => throw new Exception(s"db object expected but has '${other.getClass.getName}'")
    }
  }

  given localeFormat: MongoFormat[Locale] = new MongoFormat[Locale] {
    override def toMongoValue(a: Locale): Any = a.toLanguageTag
    override def fromMongoValue(any: Any): Locale = any match {
      case s: String =>
        s match {
          case LangTag(langTag) => langTag
          case _ =>
            if (LangTag.unapply(s).isEmpty)
              throw new Exception("Undefined locale is not allowed")
            else
              throw new Exception(LangTag.invalidLangTagMessage(s))
        }
      case _ =>
        throw new Exception(
          s"Locale is expected to be of type String but has '${any.getClass.getName}'")
    }
  }

  private def field[A](name: String, dbo: BSONObject)(implicit format: MongoFormat[A]): A =
    format.fromMongoValue(dbo.get(name))
}
