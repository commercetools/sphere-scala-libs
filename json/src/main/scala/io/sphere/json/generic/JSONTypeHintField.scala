package io.sphere.json.generic

import scala.annotation.StaticAnnotation

case class JSONTypeHintField2(value: String = JSONTypeHintField2.defaultValue) extends StaticAnnotation

object JSONTypeHintField2 {
  final val defaultValue: String = "type"
}
