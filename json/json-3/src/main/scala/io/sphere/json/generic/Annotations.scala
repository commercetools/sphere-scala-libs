package io.sphere.json.generic

import scala.annotation.StaticAnnotation

sealed trait JSONAnnotation extends StaticAnnotation

case class JSONEmbedded() extends JSONAnnotation
case class JSONIgnore() extends JSONAnnotation
case class JSONKey(value: String) extends JSONAnnotation
case class JSONTypeHintField(value: String) extends JSONAnnotation
case class JSONTypeHint(value: String) extends JSONAnnotation
