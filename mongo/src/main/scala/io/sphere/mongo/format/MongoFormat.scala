package io.sphere.mongo.format

import com.mongodb.DBObject


/** Typeclass for types with a MongoDB (Java driver) format. */
trait MongoFormat[A] {
  def toMongoValue(a: A): Any
  def fromMongoValue(any: Any): A
  def default: Option[A] = None
}

object MongoFormat {

  @inline def apply[A](implicit format: MongoFormat[A]): MongoFormat[A] = format

  /** Puts `value` under `name` into the given `DBObject`, thereby requiring and applying
    * a MongoFormat for the value type `A`. If ''value'' is ''None'' it is (intentionally) ignored.
    *
    * @param dbo The DBObject into which to put ''value'' under ''name''.
    * @tparam A The value type, for which there must exist a MongoFormat[A] instance.
    * @return The passed dbo with the new key-value pair added. */
  def put[A: MongoFormat](dbo: DBObject)(name: String, value: A): DBObject = {
    // Ignore None's, we don't ever want to store or use nulls.
    if (value != None) dbo.put(name, MongoFormat[A].toMongoValue(value))
    dbo
  }

}