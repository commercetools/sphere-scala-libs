package io.sphere.mongo.generic

import scala.annotation.StaticAnnotation

sealed trait MongoAnnotation extends StaticAnnotation

case class MongoEmbedded() extends MongoAnnotation
case class MongoIgnore() extends MongoAnnotation
case class MongoKey(value: String) extends MongoAnnotation
case class MongoTypeHintField(value: String) extends MongoAnnotation
case class MongoTypeHint(value: String) extends MongoAnnotation
