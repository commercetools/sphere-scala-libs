package io.sphere.mongo.generic

import scala.annotation.StaticAnnotation

sealed trait MongoAnnotation extends StaticAnnotation

case class MongoEmbedded() extends MongoAnnotation
case class MongoIgnore() extends MongoAnnotation
case class MongoKey(newFieldName: String) extends MongoAnnotation
case class MongoTypeHintField(typeDiscriminator: String) extends MongoAnnotation
case class MongoTypeHint(newClassName: String) extends MongoAnnotation
