package io.sphere.mongo

import io.sphere.mongo.generic
import io.sphere.mongo.format.MongoFormat

def toMongo[A: MongoFormat](a: A): Any = summon[MongoFormat[A]].toMongoValue(a)

def fromMongo[A: MongoFormat](any: Any): A = summon[MongoFormat[A]].fromMongoValue(any)
