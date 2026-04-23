package io.sphere.json

class JSONException(msg: String) extends RuntimeException(msg)

sealed abstract class JSONError
case class JSONFieldError(path: List[String], message: String) extends JSONError {
  override def toString = path.mkString(" -> ") + ": " + message
}
case class JSONParseError(message: String) extends JSONError {
  override def toString = message
}
