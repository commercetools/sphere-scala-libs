package io.sphere.mongo

import _root_.cats.Invariant
import io.sphere.mongo.format.MongoFormat

/**
 * Cats instances for [[MongoFormat]]
 */
package object catsinstances extends MongoFormatInstances

trait MongoFormatInstances {
  implicit val catsInvariantForMongoFormat: Invariant[MongoFormat] =
    new MongoFormatInvariant
}

class MongoFormatInvariant extends Invariant[MongoFormat] {
  override def imap[A, B](fa: MongoFormat[A])(f: A => B)(g: B => A): MongoFormat[B] = new MongoFormat[B] {
    override def toMongoValue(b: B): Any = fa.toMongoValue(g(b))
    override def fromMongoValue(any: Any): B = f(fa.fromMongoValue(any))
    override def fields: Set[String] = fa.fields
  }
}
