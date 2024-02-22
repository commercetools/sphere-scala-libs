package io.sphere.mongo.generic

import scala.quoted.*

def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
  //  val qq = summon[Quotes]
  //  import qq.reflect.*
  import quotes.reflect.*

  val tree: Tree = x.asTerm

  println(s"----- ${tree.show(using Printer.TreeStructure)}")
  x

inline def inspect(inline x: Any): Any = ${ inspectCode('x) }