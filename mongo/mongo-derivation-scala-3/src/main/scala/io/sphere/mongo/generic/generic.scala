package io.sphere.mongo.generic

import io.sphere.mongo.format.MongoFormat

import scala.quoted.*

inline def deriveMongoFormat[A]: MongoFormat[A] = ${ deriveMongoFormatImpl }

def deriveMongoFormatImpl[A](using Type[A], Quotes): Expr[MongoFormat[A]] = {
  val q = summon[Quotes]
  import q.reflect.*
  val t = TypeRepr.of[A]



  if(t <:< TypeRepr.of[Enumeration#Value]) then
//    Apply(
//      '{io.sphere.mongo.generic}
//      Select(Select(Select(Select(Select(Ident("io"), "sphere"), "mongo"), "generic"), "generic$package"), "mongoEnum"), List(Ident(t.asTerm))
//    )
    '{ dummyFormat[A] }
  else
    println(".......")
    '{ dummyFormat[A] }
}

//Apply(Select(Select(Select(Select(Select(Ident("io"), "sphere"), "mongo"), "generic"), "generic$package"), "mongoEnum"), List(Ident("Color")))

def dummyFormat[A]: MongoFormat[A] = new MongoFormat[A]:
  override def toMongoValue(a: A): Any = ???
  override def fromMongoValue(any: Any): A = ???


def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
  def toMongoValue(a: e.Value): Any = a.toString
  def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
}




def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
//  val qq = summon[Quotes]
//  import qq.reflect.*
  import quotes.reflect.*

  val tree: Tree = x.asTerm

  println(s"----- ${tree.show(using Printer.TreeStructure)}")
  x

inline def inspect(inline x: Any): Any = ${ inspectCode('x) }