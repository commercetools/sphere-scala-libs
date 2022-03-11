package io.sphere.json.generic

import scala.annotation.StaticAnnotation

case class JSONTypeHintField(value: String = JSONTypeHintField.defaultValue)
    extends StaticAnnotation

object JSONTypeHintField {
  final val defaultValue: String = "type"
}
