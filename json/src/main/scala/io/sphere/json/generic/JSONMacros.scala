package io.sphere.json
package generic

private[generic] object JSONMacros {
  import scala.reflect.macros.Context

  private def collectKnownSubtypes(c: Context)(s: c.universe.Symbol): Set[c.universe.Symbol] = {
    import c.universe._

    if (s.isModule || s.isModuleClass) Set(s)
    else if (s.isClass) {
      val cs = s.asClass
      if (cs.isCaseClass) Set(cs)
      else if ((cs.isTrait || cs.isAbstractClass) && cs.isSealed)
        cs.knownDirectSubclasses.flatMap(collectKnownSubtypes(c)(_))
      else Set.empty
    } else Set.empty
  }

  def jsonProductApply(c: Context)(tpe: c.universe.Type, classSym: c.universe.ClassSymbol): c.universe.Tree = {
    import c.universe._

    val symbol = tpe.typeSymbol
    val argList = classSym.toType.member(nme.CONSTRUCTOR).asMethod.paramss.head

    val (argDefs, args) = (for ((a, i) <- argList.zipWithIndex) yield {
      val argType = classSym.toType.member(a.name).typeSignatureIn(tpe)
      val argTree = ValDef(Modifiers(Flag.PARAM), newTermName("x" + i), TypeTree(argType), EmptyTree)
      (argTree, Ident(newTermName("x" + i)))
    }).unzip

    if (classSym.isCaseClass && !classSym.isModuleClass) {
      val applyBlock = Block(List(), Function(
        argDefs,
        Apply(Select(Ident(classSym.companionSymbol), newTermName("apply")), args)
      ))
      Apply(
        Select(
          reify(io.sphere.json.generic.`package`).tree,
          newTermName("jsonProduct")
        ),
        List(applyBlock)
      )
    } else if (classSym.isCaseClass && classSym.isModuleClass) {
      Apply(
        Select(
          reify(io.sphere.json.generic.`package`).tree,
          newTermName("jsonProduct0")
        ),
        List(Ident(classSym.name.toTermName))
      )
    } else if (classSym.isModuleClass) {
      Apply(
        Select(
          reify(io.sphere.json.generic.`package`).tree,
          newTermName("jsonSingleton")
        ),
        List(Ident(classSym.name.toTermName))
      )
    } else c.abort(c.enclosingPosition, "Not a case class or (case) object")
  }

  def deriveSingletonJSON_impl[A: c.WeakTypeTag](c: Context): c.Expr[JSON[A]] = {
    import c.universe._

    val tpe = weakTypeOf[A]
    val symbol = tpe.typeSymbol

    def singletonTree(classSym: c.universe.ClassSymbol): Tree =
      if (classSym.isModuleClass) {
        Apply(
          Select(
            reify(io.sphere.json.generic.`package`).tree,
            newTermName("jsonSingleton")
          ),
          List(Ident(classSym.name.toTermName))
        )
      } else c.abort(c.enclosingPosition, "Only case Objects are supported.")

    if (!symbol.isClass)
      c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class."
    ) else if (!symbol.asClass.isSealed)
      c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class."
    ) else {
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
              c.abort(c.enclosingPosition, "Types with type parameters cannot (yet) be derived as part of a sum type")
            else {
              ValDef(
                Modifiers(Flag.IMPLICIT),
                newTermName("json" + i),
                AppliedTypeTree(
                  Ident(newTypeName("JSON")),
                  List(Ident(symbol))
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
                  newTermName("jsonSingletonEnumSwitch")
                ),
                idents
              ),
              List(reify(Nil).tree)
            )
          )
        )
      }
    }
  }

  def deriveJSON_impl[A: c.WeakTypeTag](c: Context): c.Expr[JSON[A]] = {
    import c.universe._

    val tpe = weakTypeOf[A]
    val symbol = tpe.typeSymbol

    if (tpe <:< weakTypeOf[Enumeration#Value]) {
      val TypeRef(pre, _, _) = tpe
      c.Expr[JSON[A]](Apply(
        Select(
          reify(io.sphere.json.generic.`package`).tree,
          newTermName("jsonEnum")
        ),
        List(Ident(pre.typeSymbol.name.toTermName))
      ))
    } else if (symbol.isClass && (symbol.asClass.isCaseClass || symbol.asClass.isModuleClass))
      // product type or singleton
      c.Expr[JSON[A]](jsonProductApply(c)(tpe, symbol.asClass))
    else {
      // sum type
      if (!symbol.isClass) c.abort(
        c.enclosingPosition,
        "Can only enumerate values of a sealed trait or class."
      ) else if (!symbol.asClass.isSealed) c.abort(
        c.enclosingPosition,
        "Can only enumerate values of a sealed trait or class."
      ) else {
        val subtypes = collectKnownSubtypes(c)(symbol)
        val idents = Ident(symbol.name) :: subtypes.map { s =>
          if (s.isModuleClass) New(TypeTree(s.asClass.toType)) else Ident(s.name)
        }.toList

        if (idents.size == 1)
          c.abort(c.enclosingPosition, "Subtypes not found.")
        else if (idents.size < 3)
          c.abort(c.enclosingPosition, "At least 2 subtypes in type switch required.")
        else {
          val instanceDefs = subtypes.zipWithIndex.collect {
            case (symbol, i) if symbol.isClass && symbol.asClass.isCaseClass =>
              if (symbol.asClass.typeParams.nonEmpty) {
                c.abort(c.enclosingPosition, "Types with type parameters cannot (yet) be derived as part of a sum type")
              } else {
                ValDef(
                  Modifiers(Flag.IMPLICIT),
                  newTermName("json" + i),
                  AppliedTypeTree(
                    Ident(newTypeName("JSON")),
                    List(Ident(symbol))
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
                    newTermName("jsonTypeSwitch")
                  ),
                  idents
                ),
                List(reify(Nil).tree)
              )
            )
          )
        }
      }
    }
  }
}
