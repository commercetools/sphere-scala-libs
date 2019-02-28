package io.sphere.mongo.format

import java.util.{Currency, UUID}
import java.util.regex.Pattern

import com.mongodb.{BasicDBList, DBObject}
import io.sphere.util.{BaseMoney, HighPrecisionMoney, Money}
import org.bson.{BSONObject, BasicBSONObject}
import org.bson.types.ObjectId

object DefaultMongoFormats extends DefaultMongoFormats {
  val someNone = Some(None)
}

// Represents an absent value for a field that should be not serialized.
private [mongo] object MongoNothing

/**
  * [[MongoFormat]] for standard scala/mongo types
  */
trait DefaultMongoFormats {

  /** A generic format for types that are understood "as-is" by the mongo-java-driver. */
  private final class NativeMongoFormat[A] extends MongoFormat[A] {
    def toMongoValue(a: A): Any = a
    def fromMongoValue(any: Any): A = any.asInstanceOf[A]
  }

  implicit val uuidFormat: MongoFormat[UUID] = new NativeMongoFormat[UUID]
  implicit val objectIdFormat: MongoFormat[ObjectId] = new NativeMongoFormat[ObjectId]
  implicit val stringFormat: MongoFormat[String] = new NativeMongoFormat[String]
  implicit val shortFormat: MongoFormat[Short] = new NativeMongoFormat[Short]
  implicit val intFormat: MongoFormat[Int] = new NativeMongoFormat[Int]
  implicit val longFormat: MongoFormat[Long] = new MongoFormat[Long] {
    private val native = new NativeMongoFormat[Long]
    override def toMongoValue(a: Long) = native.toMongoValue(a)
    override def fromMongoValue(any: Any) =
      any match {
        // a Long can read from an Int (for example, old aggregates version)
        case i: Int ⇒ intFormat.fromMongoValue(i)
        case _ ⇒ native.fromMongoValue(any)
      }
  }

  implicit val floatFormat: MongoFormat[Float] = new NativeMongoFormat[Float]
  implicit val doubleFormat: MongoFormat[Double] = new NativeMongoFormat[Double]
  implicit val booleanFormat: MongoFormat[Boolean] = new NativeMongoFormat[Boolean]
  implicit val patternFormat: MongoFormat[Pattern] = new NativeMongoFormat[Pattern]

  implicit def optionFormat[@specialized A](implicit f: MongoFormat[A]): MongoFormat[Option[A]] = new MongoFormat[Option[A]] {
    import scala.collection.JavaConverters._
    override def toMongoValue(a: Option[A]) = a match {
      case Some(aa) => f.toMongoValue(aa)
      case None => MongoNothing
    }
    override def fromMongoValue(any: Any) = {
      Option(any) match {
        case None => None
        case Some(dbo: DBObject) if fields.nonEmpty && dbo.keySet().asScala.forall(t ⇒ !fields.contains(t)) => None
        case Some(x) => Some(f.fromMongoValue(x))
      }
    }

    override def default: Option[Option[A]] = DefaultMongoFormats.someNone
    override val fields = f.fields
  }

  implicit def vecFormat[@specialized A](implicit f: MongoFormat[A]): MongoFormat[Vector[A]] = new MongoFormat[Vector[A]] {
    import scala.collection.JavaConverters._
    override def toMongoValue(a: Vector[A]) = {
      val m = new BasicDBList()
      m.addAll(a.map(f.toMongoValue(_).asInstanceOf[AnyRef]).asJavaCollection)
      m
    }
    override def fromMongoValue(any: Any): Vector[A] = {
      any match {
        case l: BasicDBList =>
          val it = l.asInstanceOf[BasicDBList].iterator()
          it.asScala.map(f.fromMongoValue).toVector
        case _ => throw new Exception(s"cannot read value from ${any.getClass.getName}")
      }
    }
  }

  implicit def listFormat[@specialized A](implicit f: MongoFormat[A]): MongoFormat[List[A]] = new MongoFormat[List[A]] {
    import scala.collection.JavaConverters._
    override def toMongoValue(a: List[A]) = {
      val m = new BasicDBList()
      m.addAll(a.map(f.toMongoValue(_).asInstanceOf[AnyRef]).asJavaCollection)
      m
    }
    override def fromMongoValue(any: Any): List[A] = {
      any match {
        case l: BasicDBList =>
          val it = l.asInstanceOf[BasicDBList].iterator()
          it.asScala.map(f.fromMongoValue).toList
        case _ => throw new Exception(s"cannot read value from ${any.getClass.getName}")
      }
    }
  }

  implicit def setFormat[@specialized A](implicit f: MongoFormat[A]): MongoFormat[Set[A]] = new MongoFormat[Set[A]] {
    import scala.collection.JavaConverters._
    override def toMongoValue(a: Set[A]) = {
      val m = new BasicDBList()
      m.addAll(a.map(f.toMongoValue(_).asInstanceOf[AnyRef]).asJavaCollection)
      m
    }
    override def fromMongoValue(any: Any): Set[A] = {
      any match {
        case l: BasicDBList =>
          val it = l.asInstanceOf[BasicDBList].iterator()
          it.asScala.map(f.fromMongoValue).toSet
        case _ => throw new Exception(s"cannot read value from ${any.getClass.getName}")
      }
    }
  }

  implicit def mapFormat[@specialized A](implicit f: MongoFormat[A]): MongoFormat[Map[String, A]] = new MongoFormat[Map[String, A]] {
    override def toMongoValue(map: Map[String, A]): Any = {
      map.foldLeft(new BasicBSONObject()) { case (dbo, (k, v)) => dbo.append(k, f.toMongoValue(v)) }
    }

    override def fromMongoValue(any: Any): Map[String, A] = {
      import scala.language.existentials

      val map: java.util.Map[_, _] = any match {
        case b: BasicBSONObject => b // avoid instantiating a new map
        case dbo: DBObject => dbo.toMap
        case other => throw new Exception(s"cannot read value from ${other.getClass.getName}")
      }
      val builder = Map.newBuilder[String, A]
      val iter = map.entrySet().iterator()
      while (iter.hasNext) {
        val entry = iter.next()
        val k = entry.getKey.asInstanceOf[String]
        val v = f.fromMongoValue(entry.getValue)
        builder += (k -> v)
      }
      builder.result()
    }
  }

  implicit val currencyFormat: MongoFormat[Currency] = new MongoFormat[Currency] {
    val failMsg = "ISO 4217 code JSON String expected"

    override def toMongoValue(c: Currency): Any = c.getCurrencyCode
    override def fromMongoValue(any: Any): Currency = any match {
      case s: String =>
        try {
          Currency.getInstance(s)
        } catch {
          case _: IllegalArgumentException => throw new Exception(failMsg)
        }
    }
  }

  implicit val moneyFormat: MongoFormat[Money] = new MongoFormat[Money] {
    import Money._

    override val fields = Set(CentAmountField, CurrencyCodeField)

    override def toMongoValue(m: Money): Any = {
      new BasicBSONObject()
        .append(BaseMoney.TypeField, m.`type`)
        .append(CurrencyCodeField, currencyFormat.toMongoValue(m.currency))
        .append(CentAmountField, longFormat.toMongoValue(m.centAmount))
        .append(FractionDigitsField, m.currency.getDefaultFractionDigits)
    }

    override def fromMongoValue(any: Any): Money = any match {
      case dbo: BSONObject =>
        Money.fromCentAmount(
          field[Long](CentAmountField, dbo),
          field[Currency](CurrencyCodeField, dbo))
      case other => throw new Exception(s"db object expected but has '${other.getClass.getName}'")
    }
  }

  implicit val highPrecisionMoneyFormat: MongoFormat[HighPrecisionMoney] = new MongoFormat[HighPrecisionMoney] {
    import HighPrecisionMoney._

    override val fields = Set(PreciseAmountField, CurrencyCodeField, FractionDigitsField)

    override def toMongoValue(m: HighPrecisionMoney): Any = {
      new BasicBSONObject()
        .append(BaseMoney.TypeField, m.`type`)
        .append(CurrencyCodeField, currencyFormat.toMongoValue(m.currency))
        .append(CentAmountField, longFormat.toMongoValue(m.centAmount))
        .append(PreciseAmountField, longFormat.toMongoValue(m.preciseAmountAsLong))
        .append(FractionDigitsField, m.fractionDigits)
    }
    override def fromMongoValue(any: Any): HighPrecisionMoney = any match {
      case dbo: BSONObject =>
        HighPrecisionMoney.fromPreciseAmount(
          field[Long](PreciseAmountField, dbo),
          field[Int](FractionDigitsField, dbo),
          field[Currency](CurrencyCodeField, dbo),
          field[Option[Long]](CentAmountField, dbo)
        ).fold(nel => throw new Exception(nel.toList.mkString(", ")), identity)

      case other => throw new Exception(s"db object expected but has '${other.getClass.getName}'")
    }
  }

  implicit val baseMoneyFormat: MongoFormat[BaseMoney] = new MongoFormat[BaseMoney] {
    override def toMongoValue(a: BaseMoney): Any = a match {
      case m: Money => moneyFormat.toMongoValue(m)
      case m: HighPrecisionMoney => highPrecisionMoneyFormat.toMongoValue(m)
    }
    override def fromMongoValue(any: Any): BaseMoney = any match {
      case dbo: BSONObject =>
        Option(dbo.get(BaseMoney.TypeField)).map(stringFormat.fromMongoValue) match {
          case None => moneyFormat.fromMongoValue(any)
          case Some(Money.TypeName) => moneyFormat.fromMongoValue(any)
          case Some(HighPrecisionMoney.TypeName) => highPrecisionMoneyFormat.fromMongoValue(any)
          case Some(tpe) => throw new Exception(s"Unknown money type '$tpe'. Available types are: '${Money.TypeName}', '${HighPrecisionMoney.TypeName}'.")
        }
      case other => throw new Exception(s"db object expected but has '${other.getClass.getName}'")
    }
  }


  private def field[A](name: String, dbo: BSONObject)(implicit format: MongoFormat[A]): A =
    format.fromMongoValue(dbo.get(name))
}
