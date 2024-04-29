package io.sphere.mongo.generic

import scala.quoted.{Expr, Quotes, Type, Varargs}

private type MA = MongoAnnotation

case class Field(name: String, embedded: Boolean, ignored: Boolean, mongoKey: Option[MongoKey]) {
  val fieldName: String = mongoKey.map(_.newFieldName).getOrElse(name)
}
case class Annotations(
    name: String,
    forType: Vector[MA],
    byField: Map[String, Vector[MA]],
    fields: Vector[Field]
)

case class AllAnnotations(
    top: Annotations,
    subtypes: Map[String, Annotations]
)

// TODO this can probably be simplifed later
class AnnotationReader(using q: Quotes):
  import q.reflect.*

  def readCaseClassMetaData[T: Type]: Expr[Annotations] = {
    val sym = TypeRepr.of[T].typeSymbol
    topAnnotations(sym)
  }

  def allAnnotations[T: Type]: Expr[AllAnnotations] = {
    val sym = TypeRepr.of[T].typeSymbol

    '{
      AllAnnotations(
        top = ${ topAnnotations(sym) },
        subtypes = ${ subtypeAnnotations(sym) }
      )
    }
  }

  private def annotationTree(tree: Tree): Option[Expr[MA]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MA]).map(_.asExprOf[MA])

  private def findEmbedded(tree: Tree): Boolean =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MongoEmbedded]).isDefined

  private def findIgnored(tree: Tree): Boolean =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MongoIgnore]).isDefined

  private def findKey(tree: Tree): Option[Expr[MongoKey]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MongoKey]).map(_.asExprOf[MongoKey])

  private def collectFieldInfo(s: Symbol): Expr[Field] =
    val embedded = Expr(s.annotations.exists(findEmbedded))
    val ignored = Expr(s.annotations.exists(findIgnored))
    val name = Expr(s.name)
    s.annotations.map(findKey).find(_.isDefined).flatten match {
      case Some(k) =>
        '{ Field(name = $name, embedded = $embedded, ignored = $ignored, mongoKey = Some($k)) }
      case None =>
        '{ Field(name = $name, embedded = $embedded, ignored = $ignored, mongoKey = None) }
    }

  private def fieldAnnotations(s: Symbol): Expr[(String, Vector[MA])] =
    val annots = Varargs(s.annotations.flatMap(annotationTree))
    val name = Expr(s.name)

    '{ $name -> Vector($annots*) }
  end fieldAnnotations

  private def topAnnotations(sym: Symbol): Expr[Annotations] =
    val topAnns = Varargs(sym.annotations.flatMap(annotationTree))
    val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
    val fieldAnns = Varargs(caseParams.map(fieldAnnotations))
    val fields = Varargs(caseParams.map(collectFieldInfo))
    val name = Expr(sym.name)

    '{
      Annotations(
        name = $name,
        forType = Vector($topAnns*),
        byField = Map($fieldAnns*),
        fields = Vector($fields*)
      )
    }
  end topAnnotations

  private def subtypeAnnotation(sym: Symbol): Expr[(String, Annotations)] =
    val name = Expr(sym.name)
    val annots = topAnnotations(sym)
    '{ ($name, $annots) }
  end subtypeAnnotation

  private def subtypeAnnotations(sym: Symbol): Expr[Map[String, Annotations]] =
    val subtypes = Varargs(sym.children.map(subtypeAnnotation))
    '{ Map($subtypes*) }

end AnnotationReader
