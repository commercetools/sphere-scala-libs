package io.sphere.json.generic

import scala.annotation.StaticAnnotation

sealed trait JSONAnnotation extends StaticAnnotation

class JSONEmbedded() extends JSONAnnotation
class JSONIgnore() extends JSONAnnotation
class JSONKey(val value: String) extends JSONAnnotation
class JSONTypeHintField(val value: String) extends JSONAnnotation
class JSONTypeHint(val value: String) extends JSONAnnotation
