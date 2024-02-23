package io.sphere.mongo.generic

import io.sphere.mongo.format.MongoFormat

import scala.quoted.*

inline def deriveMongoFormat[A]: MongoFormat[A] = ${ deriveMongoFormatImpl }

def deriveMongoFormatImpl[A](using Type[A], Quotes): Expr[MongoFormat[A]] =
  val q = summon[Quotes]
  import q.reflect.*
  val typeRepr = TypeRepr.of[A]
  val symbol = TypeTree.of[A].symbol

  if typeRepr <:< TypeRepr.of[Enumeration#Value] then
    val enumTerm = typeRepr match
      case TypeRef(tr: TermRef, _) => tr
      case _ => report.errorAndAbort("no Enumeration found")
    Apply(
      Ref(Symbol.requiredMethod("io.sphere.mongo.generic.mongoEnum")),
      Ident(enumTerm) :: Nil
    ).asExprOf[MongoFormat[A]]

  else if symbol.flags.is(Flags.Case) && symbol.flags.is(Flags.Module) then
    // not sure if this case is ever used, at least it's not tested in this library
    val moduleName = typeRepr match {
      case TermRef(_, module) => module
      case _ => report.errorAndAbort("type does not refer to a stable value")
    }
    Apply( 
      TypeApply(
        Ref(Symbol.requiredMethod("io.sphere.mongo.generic.mongoProduct0")),
        TypeIdent(typeRepr.typeSymbol) :: Nil
      ),
      Ident(TermRef(typeRepr, moduleName)) :: Nil
    ).asExprOf[MongoFormat[A]]

  else if (symbol.flags.is(Flags.Case))
    println(s"Hello Case! $symbol")
    '{ dummyFormat[A] }

  else if (symbol.flags.is(Flags.Module))
    // has been unused with an implementation that called the non-existing function "mongoSingleton"
    report.errorAndAbort("MongoFormat for stand-alone modules is not supported.")

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

def mongoProduct0[T <: Product](singleton: T): MongoFormat[T] = ??? /*{
  val (typeField, typeValue) = mongoProduct0Type(singleton)
  new MongoFormat[T] {
    override def toMongoValue(a: T): Any = {
      val dbo = new BasicDBObject()
      dbo.append(typeField, typeValue)
      dbo
    }

    override def fromMongoValue(any: Any): T = any match {
      case o: BSONObject => findTypeValue(o, typeField) match {
        case Some(t) if t == typeValue => singleton
        case Some(t) => sys.error("Invalid type value '" + t + "'. Excepted '%s'".format(typeValue))
        case None => sys.error("Missing type field.")
      }
      case _ => sys.error("DB object excepted.")
    }
  }
}*/
