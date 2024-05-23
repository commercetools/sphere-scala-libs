package io.sphere.mongo

import io.sphere.mongo.generic
import io.sphere.mongo.generic.{MongoType, TypedMongoFormat}

def toMongo[A: TypedMongoFormat](a: A): MongoType =
  summon[generic.TypedMongoFormat[A]].toMongoValue(a)

def fromMongo[A: TypedMongoFormat](any: MongoType): A =
  summon[generic.TypedMongoFormat[A]].fromMongoValue(any)
