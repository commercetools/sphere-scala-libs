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
    typeHintRaw.filterNot(_.toList.forall(_ == ' '))

  val serializedName: String = typeHint.getOrElse(scalaName)
}

/** This class also works for case classes not only traits, in case of case classes only the `top`
  * field would be populated
  */
case class TraitMetaData(
    top: TypeMetaData,
    typeHintFieldRaw: Option[String],
    subtypes: Map[String, TypeMetaData]
) {
  def isTrait: Boolean = subtypes.nonEmpty

  private val defaultTypeDiscriminatorName = "type"
  val typeDiscriminator: String =
    typeHintFieldRaw.getOrElse(defaultTypeDiscriminatorName)

  val serializedNamesOfSubTypes: Map[String, String] = subtypes.map { case (scalaName, classMeta) =>
    scalaName -> classMeta.typeHint.getOrElse(scalaName)
  }
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
    val typeHint = sym.annotations.map(findTypeHint).find(_.isDefined).flatten match {
      case Some(th) => '{ Some($th) }
      case None => '{ None }
    }
    '{
      TypeMetaData(
        scalaName = $name,
        typeHintRaw = $typeHint,
        fields = Vector.empty
      )
    }
  }

  private def typeMetaData(sym: Symbol): Expr[TypeMetaData] = {
    val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
    val fields = Varargs(caseParams.zipWithIndex.map(collectFieldInfo(sym.companionModule)))
    val name =
      if (sym.flags.is(Flags.Case) && sym.flags.is(Flags.Module))
        Expr(sym.name.stripSuffix("$"))
      else
        Expr(sym.name)
    val typeHint = sym.annotations.map(findTypeHint).find(_.isDefined).flatten match {
      case Some(th) => '{ Some($th) }
      case None => '{ None }
    }

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
    val key = s.annotations.map(findKey).find(_.isDefined).flatten match {
      case Some(k) => '{ Some($k) }
      case None => '{ None }
    }
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

  def readTraitMetaData[T: Type]: Expr[TraitMetaData] = {
    val sym = TypeRepr.of[T].typeSymbol
    val typeHintField =
      sym.annotations.map(findTypeHintField).find(_.isDefined).flatten match {
        case Some(thf) => '{ Some($thf) }
        case None => '{ None }
      }

    '{
      TraitMetaData(
        top = ${ typeMetaData(sym) },
        typeHintFieldRaw = $typeHintField,
        subtypes = ${ subtypeAnnotations(sym) }
      )
    }
  }

}
