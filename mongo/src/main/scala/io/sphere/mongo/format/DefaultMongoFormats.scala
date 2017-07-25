package io.sphere.mongo.format

import java.util.UUID
import java.util.regex.Pattern

import com.mongodb.BasicDBList
import org.bson.types.ObjectId

object DefaultMongoFormats extends DefaultMongoFormats
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

  implicit def optionFormat[A](implicit f: MongoFormat[A]): MongoFormat[Option[A]] = new MongoFormat[Option[A]] {
    override def toMongoValue(a: Option[A]) = a match {
      case Some(aa) => f.toMongoValue(aa)
      case None => MongoNothing
    }
    override def fromMongoValue(any: Any) = {
      Option(any).map(f.fromMongoValue)
    }

    override def default: Option[Option[A]] = Some(None)
  }

  implicit def vecFormat[A](implicit f: MongoFormat[A]): MongoFormat[Vector[A]] = new MongoFormat[Vector[A]] {
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
}
