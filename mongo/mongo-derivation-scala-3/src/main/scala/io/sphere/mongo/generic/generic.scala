package io.sphere.mongo.generic

import io.sphere.mongo.format.MongoFormat

import scala.quoted.* // imports Quotes, Expr

inline def deriveMongoFormat[A]: MongoFormat[A] = ${ deriveMongoFormatImpl }

def deriveMongoFormatImpl[A](using Type[A], Quotes): Expr[MongoFormat[A]] = {
  '{ dummyFormat[A] }
}

def dummyFormat[A]: MongoFormat[A] = new MongoFormat[A]:
  override def toMongoValue(a: A): Any = ???
  override def fromMongoValue(any: Any): A = ???


def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
  def toMongoValue(a: e.Value): Any = a.toString
  def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
}




def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
  println(x.show)
  x

inline def inspect(inline x: Any): Any = ${ inspectCode('x) }