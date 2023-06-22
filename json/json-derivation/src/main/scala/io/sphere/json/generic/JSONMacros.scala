package io.sphere.json
package generic

import io.sphere.json.JSON

import scala.reflect.macros.blackbox

private[generic] object JSONMacros {
  private def collectKnownSubtypes(c: blackbox.Context)(
      s: c.universe.Symbol): Set[c.universe.Symbol] =
    if (s.isModule || s.isModuleClass) Set(s)
    else if (s.isClass) {
      val cs = s.asClass
      if (cs.isCaseClass) Set(cs)
      else if ((cs.isTrait || cs.isAbstract) && cs.isSealed)
        cs.knownDirectSubclasses.flatMap(collectKnownSubtypes(c)(_))
      else Set.empty
    } else Set.empty

  def jsonProductApply(c: blackbox.Context)(
      tpe: c.universe.Type,
      classSym: c.universe.ClassSymbol): c.universe.Tree = {
    import c.universe._

    if (classSym.isCaseClass && !classSym.isModuleClass) {
      val classSymType = classSym.toType
      val argList = classSymType.member(termNames.CONSTRUCTOR).asMethod.paramLists.head
      val modifiers = Modifiers(Flag.PARAM)
      val (argDefs, args) = (for ((a, i) <- argList.zipWithIndex) yield {
        val argType = classSymType.member(a.name).typeSignatureIn(tpe)
        val termName = TermName("x" + i)
        val argTree = ValDef(modifiers, termName, TypeTree(argType), EmptyTree)
        (argTree, Ident(termName))
      }).unzip

      val applyBlock = Block(
        Nil,
        Function(
          argDefs,
          Apply(Select(Ident(classSym.companion), TermName("apply")), args)
        ))
      Apply(
        Select(
          reify(io.sphere.json.generic.`package`).tree,
          TermName("jsonProduct")
        ),
        applyBlock :: Nil
      )
    } else if (classSym.isCaseClass && classSym.isModuleClass) {
      Apply(
        Select(
          reify(io.sphere.json.generic.`package`).tree,
          TermName("jsonProduct0")
        ),
        Ident(classSym.name.toTermName) :: Nil
      )
    } else if (classSym.isModuleClass) {
      Apply(
        Select(
          reify(io.sphere.json.generic.`package`).tree,
          TermName("jsonSingleton")
        ),
        Ident(classSym.name.toTermName) :: Nil
      )
    } else c.abort(c.enclosingPosition, "Not a case class or (case) object")
  }

  def deriveSingletonJSON_impl[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[JSON[A]] = {
    import c.universe._

    val tpe = weakTypeOf[A]
    val symbol = tpe.typeSymbol

    def singletonTree(classSym: c.universe.ClassSymbol): Tree =
      if (classSym.isModuleClass) {
        Apply(
          Select(
            reify(io.sphere.json.generic.`package`).tree,
            TermName("jsonSingleton")
          ),
          Ident(classSym.name.toTermName) :: Nil
        )
      } else c.abort(c.enclosingPosition, "Only case Objects are supported.")

    if (!symbol.isClass)
      c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class.")
    else if (!symbol.asClass.isSealed)
      c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class.")
    else {
      val subtypes = collectKnownSubtypes(c)(symbol)

      val idents = Ident(symbol.name) :: subtypes.map { s =>
        if (s.isModuleClass) TypeTree(s.asClass.toType) else Ident(s.name)
      }.toList

      if (idents.size == 1)
        c.abort(c.enclosingPosition, "Subtypes not found.")
      else {
        val instanceDefs = subtypes.zipWithIndex.collect {
          case (symbol, i) if symbol.isClass && symbol.asClass.isModuleClass =>
            if (symbol.asClass.typeParams.nonEmpty)
              c.abort(
                c.enclosingPosition,
                "Types with type parameters cannot (yet) be derived as part of a sum type")
            else {
              ValDef(
                Modifiers(Flag.IMPLICIT),
                TermName("json" + i),
                AppliedTypeTree(
                  Ident(TypeName("JSON")),
                  Ident(symbol) :: Nil
                ),
                singletonTree(symbol.asClass)
              )
            }
        }.toList

        c.Expr[JSON[A]](
          Block(
            instanceDefs,
            Apply(
              TypeApply(
                Select(
                  reify(io.sphere.json.generic.`package`).tree,
                  TermName("jsonSingletonEnumSwitch")
                ),
                idents
              ),
              reify(Nil).tree :: Nil
            )
          )
        )
      }
    }
  }

  def deriveJSON_impl[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[JSON[A]] = {
    import c.universe._

    val tpe = weakTypeOf[A]
    val symbol = tpe.typeSymbol

    if (tpe <:< weakTypeOf[Enumeration#Value]) {
      val TypeRef(pre, _, _) = tpe
      c.Expr[JSON[A]](
        Apply(
          Select(
            reify(io.sphere.json.generic.`package`).tree,
            TermName("jsonEnum")
          ),
          Ident(pre.typeSymbol.name.toTermName) :: Nil
        ))
    } else if (symbol.isClass && (symbol.asClass.isCaseClass || symbol.asClass.isModuleClass))
      // product type or singleton
      c.Expr[JSON[A]](jsonProductApply(c)(tpe, symbol.asClass))
    else {
      // sum type
      if (!symbol.isClass)
        c.abort(
          c.enclosingPosition,
          "Can only enumerate values of a sealed trait or class."
        )
      else if (!symbol.asClass.isSealed)
        c.abort(
          c.enclosingPosition,
          "Can only enumerate values of a sealed trait or class."
        )
      else {
        val subtypes = collectKnownSubtypes(c)(symbol)
        c.info(c.enclosingPosition, symbol.fullName + " Subtypes: " + subtypes.mkString(", "), true)
        val idents = Ident(symbol.name) :: subtypes.map { s =>
          if (s.isModuleClass) New(TypeTree(s.asClass.toType)) else Ident(s.name)
        }.toList

        if (idents.size == 1)
          c.abort(c.enclosingPosition, "Subtypes not found.")
        else {
          val instanceDefs = subtypes.zipWithIndex.collect {
            case (symbol, i) if symbol.isClass && symbol.asClass.isCaseClass =>
              if (symbol.asClass.typeParams.nonEmpty) {
                c.abort(
                  c.enclosingPosition,
                  "Types with type parameters cannot (yet) be derived as part of a sum type")
              } else {
                ValDef(
                  Modifiers(Flag.IMPLICIT),
                  TermName("json" + i),
                  AppliedTypeTree(
                    Ident(TypeName("JSON")),
                    Ident(symbol) :: Nil
                  ),
                  jsonProductApply(c)(tpe, symbol.asClass)
                )
              }
          }.toList

          c.Expr[JSON[A]](
            Block(
              instanceDefs,
              Apply(
                TypeApply(
                  Select(
                    reify(io.sphere.json.generic.`package`).tree,
                    TermName("jsonTypeSwitch")
                  ),
                  idents
                ),
                reify(Nil).tree :: Nil
              )
            )
          )
        }
      }
    }
  }
}
