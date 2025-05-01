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

case class TypeMetaData(
    name: String,
    typeHintRaw: Option[JSONTypeHint],
    fields: Vector[Field]
) {
  val typeHint: Option[String] =
    typeHintRaw.map(_.value).filterNot(_.toList.forall(_ == ' '))
}

/** This class also works for case classes not only traits, in case of case classes only the `top`
  * field would be populated
  */
case class TraitMetaData(
    top: TypeMetaData,
    typeHintFieldRaw: Option[JSONTypeHintField],
    subtypes: Map[String, TypeMetaData]
) {
  def isTrait: Boolean = subtypes.nonEmpty

  private val defaultTypeDiscriminatorName = "type"
  val typeDiscriminator: String =
    typeHintFieldRaw.map(_.value).getOrElse(defaultTypeDiscriminatorName)

  val subTypeFieldRenames: Map[String, String] = subtypes.collect {
    case (name, classMeta) if classMeta.typeHint.isDefined =>
      name -> classMeta.typeHint.get
  }
}

class AnnotationReader(using q: Quotes) {

  import q.reflect.*

  def readTypeMetaData[T: Type]: Expr[TypeMetaData] = {
    val sym = TypeRepr.of[T].typeSymbol
    typeMetaData(sym)
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
        top = ${ typeMetaData(sym) },
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

  private def typeMetaData(sym: Symbol): Expr[TypeMetaData] = {
    val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
    val fields = Varargs(caseParams.zipWithIndex.map(collectFieldInfo(sym.companionModule)))
    // Removing $ from the end of object names (productPrefix doesn't return names like that, so it's better not to have it)
    val name = Expr(sym.name.stripSuffix("$"))
    val typeHint = sym.annotations.map(findTypeHint).find(_.isDefined).flatten match {
      case Some(th) => '{ Some($th) }
      case None => '{ None }
    }

    '{
      TypeMetaData(
        name = $name,
        typeHintRaw = $typeHint,
        fields = Vector($fields*)
      )
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

}

object AnnotationReader {
  inline def readTypeMetaData[T]: TypeMetaData = ${ readTypeMetaDataImpl[T] }

  inline def readTraitMetaData[T]: TraitMetaData = ${ readTraitMetaDataImpl[T] }

  private def readTypeMetaDataImpl[T: Type](using Quotes): Expr[TypeMetaData] =
    AnnotationReader().readTypeMetaData[T]

  private def readTraitMetaDataImpl[T: Type](using Quotes): Expr[TraitMetaData] =
    AnnotationReader().readTraitMetaData[T]
}
