package io.sphere.mongo.format

import org.bson.BSONObject

import scala.annotation.implicitNotFound

/** Typeclass for types with a MongoDB (Java driver) format. */
@implicitNotFound("Could not find an instance of MongoFormat for ${A}")
trait MongoFormat[@specialized A] extends Serializable {
  def toMongoValue(a: A): Any
  def fromMongoValue(any: Any): A
  def default: Option[A] = None
  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = MongoFormat.emptyFieldsSet
}

object MongoFormat {

  private[MongoFormat] val emptyFieldsSet: Set[String] = Set.empty

  @inline def apply[A](implicit instance: MongoFormat[A]): MongoFormat[A] = instance

  /** Puts `value` under `name` into the given `BSONObject`, thereby requiring and applying
    * a MongoFormat for the value type `A`. If ''value'' is ''None'' it is (intentionally) ignored.
    *
    * @param dbo The BSONObject into which to put ''value'' under ''name''.
    * @tparam A The value type, for which there must exist a MongoFormat[A] instance.
    * @return The passed dbo with the new key-value pair added. */
  def put[A: MongoFormat](dbo: BSONObject)(name: String, value: A): BSONObject = {
    // Ignore None's, we don't ever want to store or use nulls.
    if (value != None) dbo.put(name, MongoFormat[A].toMongoValue(value))
    dbo
  }

}
