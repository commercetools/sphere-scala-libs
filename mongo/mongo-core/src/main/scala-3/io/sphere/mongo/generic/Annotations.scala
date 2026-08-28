package io.sphere.mongo.generic

import scala.annotation.StaticAnnotation

sealed trait MongoAnnotation extends StaticAnnotation

case class MongoEmbedded() extends MongoAnnotation
case class MongoIgnore() extends MongoAnnotation
case class MongoKey(value: String) extends MongoAnnotation
case class MongoTypeHintField(value: String) extends MongoAnnotation
case class MongoTypeHint(value: String) extends MongoAnnotation

// No-op in Scala 3 (derivation picks up provided implicits automatically); exists so the
// shared test spec can keep the annotation it needs under Scala 2.
class MongoProvidedFormatter extends StaticAnnotation
