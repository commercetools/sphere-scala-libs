package io.sphere.mongo

package object format {

  def toMongo[A: MongoFormat](a: A): AnyRef = MongoFormat[A].toMongoValue(a).asInstanceOf[AnyRef]
  def fromMongo[A: MongoFormat](any: Any): A = MongoFormat[A].fromMongoValue(any)

}
