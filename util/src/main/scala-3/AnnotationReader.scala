package io.sphere.util

import scala.quoted.{Expr, Quotes, Type, Varargs}

case class Field(
    scalaName: String,
    embedded: Boolean,
    ignored: Boolean,
    key: Option[String],
    defaultArgument: Option[Any]) {
  val serializedName: String = key.getOrElse(scalaName)
}

case class TypeMetaData(
    scalaName: String,
    typeHintRaw: Option[String],
    fields: Vector[Field]
) {
  val typeHint: Option[String] =
    typeHintRaw.filterNot(_.trim.isEmpty)
}

case class TraitMetaData(
    typeHintFieldRaw: Option[String],
    subtypes: Map[String, TypeMetaData]
) {
  val typeDiscriminator: String =
    typeHintFieldRaw.getOrElse(TraitMetaData.defaultTypeDiscriminatorName)

  val serializedNamesOfSubTypes: Map[String, String] = subtypes.map { case (scalaName, classMeta) =>
    scalaName -> classMeta.typeHint.getOrElse(scalaName)
  }
}

object TraitMetaData {
  val defaultTypeDiscriminatorName = "type"
}

class AnnotationReader(using q: Quotes)(
    embeddedExists: q.reflect.Tree => Boolean,
    ignoredExists: q.reflect.Tree => Boolean,
    findKey: q.reflect.Tree => Option[Expr[String]],
    findTypeHint: q.reflect.Tree => Option[Expr[String]],
    findTypeHintField: q.reflect.Tree => Option[Expr[String]]
) {
  import q.reflect.*

  def readTypeMetaData[T: Type]: Expr[TypeMetaData] = {
    val tpe = TypeRepr.of[T]
    val termSym = tpe.termSymbol
    val typeSym = tpe.typeSymbol
    if (termSym.flags.is(Flags.Enum) && typeSym.flags.is(Flags.Enum))
      typeMetaDataForEnumObjects(termSym)
    else
      typeMetaData(typeSym)
  }

  private def typeMetaDataForEnumObjects(sym: Symbol): Expr[TypeMetaData] = {
    val name = Expr(sym.name)
    val typeHint = collectFirstAnnotation(sym, findTypeHint)

    '{
      TypeMetaData(
        scalaName = $name,
        typeHintRaw = $typeHint,
        fields = Vector.empty
      )
    }
  }

  private def scalaName(sym: Symbol): String =
    if (sym.flags.is(Flags.Case) && sym.flags.is(Flags.Module)) sym.name.stripSuffix("$")
    else sym.name

  /** The `serializedName` of `T`, without building the rest of its metadata. Cheap enough to expand
    * once per subtype of a type switch, which reading the whole `TraitMetaData` is not.
    */
  def readSerializedName[T: Type]: Expr[String] = {
    val sym = TypeRepr.of[T].typeSymbol
    val name = Expr(scalaName(sym))
    sym.annotations.flatMap(findTypeHint).headOption match {
      case Some(hint) => '{ if ($hint.trim.isEmpty) $name else $hint }
      case None => name
    }
  }

  /** The `typeDiscriminator` of `T`, without building the rest of its metadata. */
  def readTypeDiscriminator[T: Type]: Expr[String] = {
    val sym = TypeRepr.of[T].typeSymbol
    checkSubtypeDiscriminators(sym)
    sym.annotations
      .flatMap(findTypeHintField)
      .headOption
      .getOrElse(Expr(TraitMetaData.defaultTypeDiscriminatorName))
  }

  private def typeMetaData(sym: Symbol): Expr[TypeMetaData] = {
    val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
    val fields = Varargs(caseParams.zipWithIndex.map(collectFieldInfo(sym.companionModule)))
    val name = Expr(scalaName(sym))
    val typeHint = collectFirstAnnotation(sym, findTypeHint)

    '{
      TypeMetaData(
        scalaName = $name,
        typeHintRaw = $typeHint,
        fields = Vector($fields*)
      )
    }
  }

  private def collectFieldInfo(companion: Symbol)(s: Symbol, paramIdx: Int): Expr[Field] = {
    val embedded = Expr(s.annotations.exists(embeddedExists))
    val ignored = Expr(s.annotations.exists(ignoredExists))
    val name = Expr(s.name)
    val key = collectFirstAnnotation(s, findKey)
    val defArgOpt = companion
      .methodMember(s"$$lessinit$$greater$$default$$${paramIdx + 1}")
      .headOption
      .map(dm => Ref(dm).asExprOf[Any]) match {
      case Some(k) => '{ Some($k) }
      case None => '{ None }
    }

    '{
      Field(
        scalaName = $name,
        embedded = $embedded,
        ignored = $ignored,
        key = $key,
        defaultArgument = $defArgOpt)
    }
  }

  private def subtypeAnnotation(sym: Symbol): Expr[(String, TypeMetaData)] = {
    val name = Expr(sym.name)
    val annots = typeMetaData(sym)
    '{ ($name, $annots) }
  }

  private def subtypeAnnotations(sym: Symbol): Expr[Map[String, TypeMetaData]] = {
    val subtypes = Varargs(sym.children.map(subtypeAnnotation))
    '{ Map($subtypes*) }
  }

  private def typeHintFieldConstant(sym: Symbol): Option[String] =
    sym.annotations
      .find(t => findTypeHintField(t).isDefined)
      .flatMap {
        case Apply(_, args) => args.collectFirst { case Literal(StringConstant(v)) => v }
        case _ => None
      }

  private def checkSubtypeDiscriminators(sym: Symbol): Unit = {
    val traitDiscriminator =
      typeHintFieldConstant(sym).getOrElse(TraitMetaData.defaultTypeDiscriminatorName)
    sym.children.foreach { child =>
      typeHintFieldConstant(child).foreach { childDiscriminator =>
        if (childDiscriminator != traitDiscriminator)
          report.errorAndAbort(
            s"Sub type '${child.name}' has a different type hint field " +
              s"('$childDiscriminator') than its super type '${sym.name}' ('$traitDiscriminator').")
      }
    }
  }

  def readTraitMetaData[T: Type]: Expr[TraitMetaData] = {
    val sym = TypeRepr.of[T].typeSymbol
    val typeHintField = collectFirstAnnotation(sym, findTypeHintField)

    checkSubtypeDiscriminators(sym)

    val subTypeAnnots = subtypeAnnotations(sym)

    '{
      TraitMetaData(
        typeHintFieldRaw = $typeHintField,
        subtypes = $subTypeAnnots
      )
    }
  }

  private def collectFirstAnnotation(
      sym: Symbol,
      find: Tree => Option[Expr[String]]): Expr[Option[String]] =
    sym.annotations.flatMap(find).headOption match {
      case Some(x) => '{ Some($x) }
      case None => '{ None }
    }

}
