package io.sphere.mongo.format

import java.util.UUID
import java.util.regex.Pattern

import com.mongodb.{BasicDBList, DBObject}
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

}
