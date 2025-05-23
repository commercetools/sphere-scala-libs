package io.sphere.json.generic

import io.sphere.util.{AnnotationReader, TraitMetaData, TypeMetaData}

import scala.quoted.{Expr, Quotes, Type}

object AnnotationReader {
  inline def readTypeMetaData[T]: TypeMetaData = ${ readTypeMetaDataImpl[T] }

  inline def readTraitMetaData[T]: TraitMetaData = ${ readTraitMetaDataImpl[T] }

  private def readTypeMetaDataImpl[T: Type](using Quotes): Expr[TypeMetaData] =
    JsonAnnotationReader().readTypeMetaData[T]

  private def readTraitMetaDataImpl[T: Type](using Quotes): Expr[TraitMetaData] =
    JsonAnnotationReader().readTraitMetaData[T]
}

class JsonAnnotationReader(using q: Quotes) {
  import q.reflect.*
  private def findAnnotation[JA <: JSONAnnotation: Type](tree: Tree): Option[Expr[Any]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[JA])

  private def embeddedExists(tree: Tree): Boolean =
    findAnnotation[JSONEmbedded](tree).isDefined

  private def ignoredExists(tree: Tree): Boolean =
    findAnnotation[JSONIgnore](tree).isDefined

  private def findKey(tree: Tree): Option[Expr[String]] =
    findAnnotation[JSONKey](tree).map(_.asExprOf[JSONKey]).map(a => '{ $a.value })

  private def findTypeHint(tree: Tree): Option[Expr[String]] =
    findAnnotation[JSONTypeHint](tree).map(_.asExprOf[JSONTypeHint]).map(a => '{ $a.value })

  private def findTypeHintField(tree: Tree): Option[Expr[String]] =
    findAnnotation[JSONTypeHintField](tree)
      .map(_.asExprOf[JSONTypeHintField])
      .map(a => '{ $a.value })

  private val annotationReader =
    new AnnotationReader(embeddedExists, ignoredExists, findKey, findTypeHint, findTypeHintField)

  export annotationReader.readTypeMetaData
  export annotationReader.readTraitMetaData
}
