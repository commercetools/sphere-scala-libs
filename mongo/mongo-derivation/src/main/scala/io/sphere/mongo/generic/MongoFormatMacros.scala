package io.sphere.mongo.generic

import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.generic.annotations.MongoProvidedFormatter

import scala.reflect.macros.blackbox

/** copy/paste from
  * https://github.com/sphereio/sphere-scala-libs/blob/master/json/src/main/scala/generic/JSONMacros.scala,
  * adapted to `MongoFormat`.
  */
private[generic] object MongoFormatMacros {
  private def collectKnownSubtypes(c: blackbox.Context)(
      s: c.universe.Symbol): Set[c.universe.Symbol] =
    if (s.isModule || s.isModuleClass) Set(s)
    else if (s.isClass) {
      val cs = s.asClass
      if (cs.isCaseClass) Set(cs)
      else if (cs.isTrait || cs.isAbstract) {
        if (cs.isSealed) {
          cs.knownDirectSubclasses.flatMap(collectKnownSubtypes(c)(_))
        } else {
          c.abort(
            c.enclosingPosition,
            s"Can only enumerate values of a sealed trait or class, failed on: '${cs.name}'"
          )
        }
      } else Set.empty
    } else Set.empty

  def mongoFormatProductApply(c: blackbox.Context)(
      tpe: c.universe.Type,
      classSym: c.universe.ClassSymbol): c.universe.Tree = {
    import c.universe._

    if (classSym.isCaseClass && !classSym.isModuleClass) {
      val argList = classSym.toType.member(termNames.CONSTRUCTOR).asMethod.paramLists.head
      val modifiers = Modifiers(Flag.PARAM)
      val (argDefs, args) = (for ((a, i) <- argList.zipWithIndex) yield {
        val argType = classSym.toType.member(a.name).typeSignatureIn(tpe)
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
          reify(io.sphere.mongo.generic.`package`).tree,
          TermName("mongoProduct")
        ),
        applyBlock :: Nil
      )
    } else if (classSym.isCaseClass && classSym.isModuleClass) {
      Apply(
        Select(
          reify(io.sphere.mongo.generic.`package`).tree,
          TermName("mongoProduct0")
        ),
        Ident(classSym.name.toTermName) :: Nil
      )
    } else if (classSym.isModuleClass) {
      Apply(
        Select(
          reify(io.sphere.mongo.generic.`package`).tree,
          TermName("mongoSingleton")
        ),
        Ident(classSym.name.toTermName) :: Nil
      )
    } else c.abort(c.enclosingPosition, "Not a case class or (case) object")
  }

  def deriveMongoFormat_impl[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[MongoFormat[A]] = {
    import c.universe._

    val tpe = weakTypeOf[A]
    val symbol = tpe.typeSymbol

    if (tpe <:< weakTypeOf[Enumeration#Value]) {
      val TypeRef(pre, _, _) = tpe
      c.Expr[MongoFormat[A]](
        Apply(
          Select(
            reify(io.sphere.mongo.generic.`package`).tree,
            TermName("mongoEnum")
          ),
          Ident(pre.typeSymbol.name.toTermName) :: Nil
        ))
    } else if (symbol.isClass && (symbol.asClass.isCaseClass || symbol.asClass.isModuleClass))
      // product type or singleton
      c.Expr[MongoFormat[A]](mongoFormatProductApply(c)(tpe, symbol.asClass))
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

        val (subtypesWithFormatter, subtypesWithNoFormatter) =
          subtypes.partition(mongoFormatExists(c))
        val singleParamTypes = subtypesWithFormatter.filter(_.asType.typeParams.length == 1)

        val idents = Ident(symbol.name) :: (subtypes -- singleParamTypes).map { s =>
          if (s.isModuleClass) New(TypeTree(s.asClass.toType)) else Ident(s.name)
        }.toList

        if (idents.size == 1)
          c.abort(c.enclosingPosition, "Subtypes not found.")
        else {
          val instanceDefs = subtypesWithNoFormatter.zipWithIndex.collect {
            case (symbol, i) if symbol.isClass && symbol.asClass.isCaseClass =>
              if (symbol.asClass.typeParams.nonEmpty) {
                c.abort(
                  c.enclosingPosition,
                  "Types with type parameters cannot (yet) be derived as part of a sum type")
              } else {
                ValDef(
                  Modifiers(Flag.IMPLICIT),
                  TermName("mongo" + i),
                  AppliedTypeTree(
                    Ident(TypeName("MongoFormat")),
                    Ident(symbol) :: Nil
                  ),
                  mongoFormatProductApply(c)(tpe, symbol.asClass)
                )
              }
          }.toList

          val typeSelectors = singleParamTypes.map { t =>
            val firstTypeParam = t.asType.typeParams.head
            // This code relies on type erasure. TypeSelectors are grouped by their Class[A]
            // But in case A has a type parameter it's erased anyway,
            // so the Map[Class[A[_].. currently cannot store multiple instances for a single type parameter type based on the type param
            // Until this changes we'd need to provide an Any instance or an upper bound instance anyway
            firstTypeParam.typeSignature match {
              case TypeBounds(_, superType) =>
                q"new io.sphere.mongo.generic.TypeSelector[$t[$superType]](${t.name.toString}, classOf[$t[$superType]])"
              case _ =>
                q"new io.sphere.mongo.generic.TypeSelector[$t[Any]](${t.name.toString}, classOf[$t[Any]])"
            }
          }.toList

          c.Expr[MongoFormat[A]](
            Block(
              instanceDefs,
              Apply(
                TypeApply(
                  Select(
                    reify(io.sphere.mongo.generic.`package`).tree,
                    TermName("mongoTypeSwitch")
                  ),
                  idents
                ),
                q"$typeSelectors" :: Nil
              )
            )
          )
        }
      }
    }
  }
  private def mongoFormatExists(c: blackbox.Context)(s: c.universe.Symbol) =
    s.annotations.exists(_.tree.tpe =:= c.universe.typeOf[MongoProvidedFormatter])
}
