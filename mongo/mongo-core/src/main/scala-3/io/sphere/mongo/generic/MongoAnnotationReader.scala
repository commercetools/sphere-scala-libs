package io.sphere.mongo.generic

import io.sphere.util.{AnnotationReader, TraitMetaData, TypeMetaData}

import scala.quoted.{Expr, Quotes, Type, Varargs}

object MongoAnnotationReader {

  inline def readTraitMetaData[T]: TraitMetaData = ${ readTraitMetaDataImpl[T] }

  inline def readTypeMetaData[T]: TypeMetaData = ${ readTypeMetaDataImpl[T] }

  private def readTypeMetaDataImpl[T: Type](using Quotes): Expr[TypeMetaData] =
    MongoAnnotationReader().readTypeMetaData[T]

  private def readTraitMetaDataImpl[T: Type](using Quotes): Expr[TraitMetaData] =
    MongoAnnotationReader().readTraitMetaData[T]
}

class MongoAnnotationReader(using q: Quotes) {
  import q.reflect.*

  private def findAnnotation[MA <: MongoAnnotation: Type](tree: Tree): Option[Expr[Any]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MA])

  private def findEmbedded(tree: Tree): Boolean =
    findAnnotation[MongoEmbedded](tree).isDefined

  private def findIgnored(tree: Tree): Boolean =
    findAnnotation[MongoIgnore](tree).isDefined

  private def findKey(tree: Tree): Option[Expr[String]] =
    findAnnotation[MongoKey](tree).map(_.asExprOf[MongoKey]).map(a => '{ $a.value })

  private def findTypeHint(tree: Tree): Option[Expr[String]] =
    findAnnotation[MongoTypeHint](tree).map(_.asExprOf[MongoTypeHint]).map(a => '{ $a.value })

  private def findMongoTypeHintField(tree: Tree): Option[Expr[String]] =
    findAnnotation[MongoTypeHintField](tree)
      .map(_.asExprOf[MongoTypeHintField])
      .map(a => '{ $a.value })

  private val annotationReader =
    new AnnotationReader(findEmbedded, findIgnored, findKey, findTypeHint, findMongoTypeHintField)
  export annotationReader.readTraitMetaData
  export annotationReader.readTypeMetaData

}
