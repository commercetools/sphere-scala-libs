package io.sphere.mongo.generic

import scala.annotation.StaticAnnotation

case class MongoTypeHintField(value: String = MongoTypeHintField.defaultValue)
    extends StaticAnnotation

object MongoTypeHintField {
  final val defaultValue: String = "type"
}
