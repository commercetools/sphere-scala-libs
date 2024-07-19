package io.sphere.json.generic

import io.sphere.json.generic.JSONAnnotation
import io.sphere.json.generic.JSONTypeHint

import scala.quoted.{Expr, Quotes, Type, Varargs}

private type MA = JSONAnnotation

case class Field(
    name: String,
    embedded: Boolean,
    ignored: Boolean,
    jsonKey: Option[JSONKey],
    defaultArgument: Option[Any]) {
  val fieldName: String = jsonKey.map(_.value).getOrElse(name)
}

case class CaseClassMetaData(
    name: String,
    typeHintRaw: Option[JSONTypeHint],
    fields: Vector[Field]
) {
  val typeHint: Option[String] =
    typeHintRaw.map(_.value).filterNot(_.toList.forall(_ == ' '))
}

case class TraitMetaData(
    top: CaseClassMetaData,
    typeHintFieldRaw: Option[JSONTypeHintField],
    subtypes: Map[String, CaseClassMetaData]
) {
  val typeDiscriminator: String = typeHintFieldRaw.map(_.value).getOrElse("type")
}

class AnnotationReader(using q: Quotes) {

  import q.reflect.*

  def readCaseClassMetaData[T: Type]: Expr[CaseClassMetaData] = {
    val sym = TypeRepr.of[T].typeSymbol
    caseClassMetaData(sym)
  }

  def readTraitMetaData[T: Type]: Expr[TraitMetaData] = {
    val sym = TypeRepr.of[T].typeSymbol
    val typeHintField =
      sym.annotations.map(findJSONTypeHintField).find(_.isDefined).flatten match {
        case Some(thf) => '{ Some($thf) }
        case None => '{ None }
      }

    '{
      TraitMetaData(
        top = ${ caseClassMetaData(sym) },
        typeHintFieldRaw = $typeHintField,
        subtypes = ${ subtypeAnnotations(sym) }
      )
    }
  }

  private def annotationTree(tree: Tree): Option[Expr[MA]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MA]).map(_.asExprOf[MA])

  private def findEmbedded(tree: Tree): Boolean =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[JSONEmbedded]).isDefined

  private def findIgnored(tree: Tree): Boolean =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[JSONIgnore]).isDefined

  private def findKey(tree: Tree): Option[Expr[JSONKey]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[JSONKey]).map(_.asExprOf[JSONKey])

  private def findTypeHint(tree: Tree): Option[Expr[JSONTypeHint]] =
    Option
      .when(tree.isExpr)(tree.asExpr)
      .filter(_.isExprOf[JSONTypeHint])
      .map(_.asExprOf[JSONTypeHint])

  private def findJSONTypeHintField(tree: Tree): Option[Expr[JSONTypeHintField]] =
    Option
      .when(tree.isExpr)(tree.asExpr)
      .filter(_.isExprOf[JSONTypeHintField])
      .map(_.asExprOf[JSONTypeHintField])

  private def collectFieldInfo(companion: Symbol)(s: Symbol, paramIdx: Int): Expr[Field] = {
    val embedded = Expr(s.annotations.exists(findEmbedded))
    val ignored = Expr(s.annotations.exists(findIgnored))
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
        name = $name,
        embedded = $embedded,
        ignored = $ignored,
        jsonKey = $key,
        defaultArgument = $defArgOpt)
    }
  }

  private def caseClassMetaData(sym: Symbol): Expr[CaseClassMetaData] = {
    val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
    val fields = Varargs(caseParams.zipWithIndex.map(collectFieldInfo(sym.companionModule)))
    val name = Expr(sym.name)
    val typeHint = sym.annotations.map(findTypeHint).find(_.isDefined).flatten match {
      case Some(th) => '{ Some($th) }
      case None => '{ None }
    }

    '{
      CaseClassMetaData(
        name = $name,
        typeHintRaw = $typeHint,
        fields = Vector($fields*)
      )
    }
  }

  private def subtypeAnnotation(sym: Symbol): Expr[(String, CaseClassMetaData)] = {
    val name = Expr(sym.name)
    val annots = caseClassMetaData(sym)
    '{ ($name, $annots) }
  }

  private def subtypeAnnotations(sym: Symbol): Expr[Map[String, CaseClassMetaData]] = {
    val subtypes = Varargs(sym.children.map(subtypeAnnotation))
    '{ Map($subtypes*) }
  }

}

object AnnotationReader {
  inline def readCaseClassMetaData[T]: CaseClassMetaData = ${ readCaseClassMetaDataImpl[T] }

  inline def readTraitMetaData[T]: TraitMetaData = ${ readTraitMetaDataImpl[T] }

  private def readCaseClassMetaDataImpl[T: Type](using Quotes): Expr[CaseClassMetaData] =
    AnnotationReader().readCaseClassMetaData[T]

  private def readTraitMetaDataImpl[T: Type](using Quotes): Expr[TraitMetaData] =
    AnnotationReader().readTraitMetaData[T]
}
