package io.sphere.mongo.generic

import io.sphere.mongo.format.MongoFormat

import scala.quoted.*

inline def deriveMongoFormat[A]: MongoFormat[A] = ${ deriveMongoFormatImpl }

def deriveMongoFormatImpl[A](using Type[A], Quotes): Expr[MongoFormat[A]] =
  val q = summon[Quotes]
  import q.reflect.*
  val typeRepr = TypeRepr.of[A]

  if(typeRepr <:< TypeRepr.of[Enumeration#Value]) then
    val enumTerm = typeRepr match
      case TypeRef(tr: TermRef, _) => tr
      case _ => report.errorAndAbort("no Enumeration found")

    val mongoEnumCall = Apply(
      Ref(Symbol.requiredMethod("io.sphere.mongo.generic.mongoEnum")),
      List(Ident(enumTerm))
    )
    mongoEnumCall.asExprOf[MongoFormat[A]]
  else
    println(".......")
    '{ dummyFormat[A] }
end deriveMongoFormatImpl


def dummyFormat[A]: MongoFormat[A] = new MongoFormat[A]:
  override def toMongoValue(a: A): Any = ???
  override def fromMongoValue(any: Any): A = ???


def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
  def toMongoValue(a: e.Value): Any = a.toString
  def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
}

