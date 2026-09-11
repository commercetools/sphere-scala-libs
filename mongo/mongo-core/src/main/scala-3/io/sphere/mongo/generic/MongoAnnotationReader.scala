package io.sphere.mongo.generic

import io.sphere.util.{AnnotationReader, TypeMetaData}

import scala.quoted.{Expr, Quotes, Type}

object MongoAnnotationReader {

  inline def readTypeMetaData[T]: TypeMetaData = ${ readTypeMetaDataImpl[T] }

  inline def readSerializedName[T]: String = ${ readSerializedNameImpl[T] }

  inline def readTypeDiscriminator[T]: String = ${ readTypeDiscriminatorImpl[T] }

  private def readTypeMetaDataImpl[T: Type](using Quotes): Expr[TypeMetaData] =
    MongoAnnotationReader().readTypeMetaData[T]

  private def readSerializedNameImpl[T: Type](using Quotes): Expr[String] =
    MongoAnnotationReader().readSerializedName[T]

  private def readTypeDiscriminatorImpl[T: Type](using Quotes): Expr[String] =
    MongoAnnotationReader().readTypeDiscriminator[T]
}

class MongoAnnotationReader(using q: Quotes) {
  import q.reflect.*

  private def findAnnotation[MA <: MongoAnnotation: Type](tree: Tree): Option[Expr[Any]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MA])

  private def embeddedExists(tree: Tree): Boolean =
    findAnnotation[MongoEmbedded](tree).isDefined

  private def ignoredExists(tree: Tree): Boolean =
    findAnnotation[MongoIgnore](tree).isDefined

  private def findKey(tree: Tree): Option[Expr[String]] =
    findAnnotation[MongoKey](tree).map(a => '{ ${ a.asExprOf[MongoKey] }.value })

  private def findTypeHint(tree: Tree): Option[Expr[String]] =
    findAnnotation[MongoTypeHint](tree).map(a => '{ ${ a.asExprOf[MongoTypeHint] }.value })

  private def findTypeHintField(tree: Tree): Option[Expr[String]] =
    findAnnotation[MongoTypeHintField](tree).map(a =>
      '{ ${ a.asExprOf[MongoTypeHintField] }.value })

  private val annotationReader =
    new AnnotationReader(embeddedExists, ignoredExists, findKey, findTypeHint, findTypeHintField)
  export annotationReader.readTypeMetaData
  export annotationReader.readSerializedName
  export annotationReader.readTypeDiscriminator

}
