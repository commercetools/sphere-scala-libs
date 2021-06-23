package io.sphere.json

import org.json4s.JsonAST.JValue

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of JSON for ${A}")
trait JSON[A] extends FromJSON[A] with ToJSON[A]

object JSON extends JSONInstances with JSONLowPriorityImplicits {
  @inline def apply[A](implicit instance: JSON[A]): JSON[A] = instance
}

trait JSONLowPriorityImplicits {
  implicit def fromJSONAndToJSON[A](implicit fromJSON: FromJSON[A], toJSON: ToJSON[A]): JSON[A] =
    new JSON[A] {
      override def read(jval: JValue): JValidation[A] = fromJSON.read(jval)
      override def write(value: A): JValue = toJSON.write(value)
    }
}

class JSONException(msg: String) extends RuntimeException(msg)

sealed abstract class JSONError
case class JSONFieldError(path: List[String], message: String) extends JSONError {
  override def toString = path.mkString(" -> ") + ": " + message
}
case class JSONParseError(message: String) extends JSONError {
  override def toString = message
}
