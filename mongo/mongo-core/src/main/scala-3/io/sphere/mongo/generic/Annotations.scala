package io.sphere.mongo.generic

import scala.annotation.StaticAnnotation

sealed trait MongoAnnotation extends StaticAnnotation

class MongoEmbedded() extends MongoAnnotation
class MongoIgnore() extends MongoAnnotation
class MongoKey(val value: String) extends MongoAnnotation
class MongoTypeHintField(val value: String) extends MongoAnnotation
class MongoTypeHint(val value: String) extends MongoAnnotation

// No-op in Scala 3 (derivation picks up provided implicits automatically); exists so the
// shared test spec can keep the annotation it needs under Scala 2.
private[generic] class MongoProvidedFormatter extends StaticAnnotation
