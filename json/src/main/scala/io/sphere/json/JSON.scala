package io.sphere.json

import cats.data.ValidatedNel
import org.json4s.JsonAST.JValue

trait JSON[A] extends FromJSON[A] with ToJSON[A]

object JSON {
  @inline def apply[A](implicit instance: JSON[A]): JSON[A] = instance

  // create a JSON type class instance from a ToJSON and a FromJSON
  implicit def JSONofToAndFrom[A](implicit toJSON: ToJSON[A], fromJSON: FromJSON[A]): JSON[A] =
    new JSON[A] {
      def write(a: A): JValue = toJSON.write(a)
      def read(jval: JValue): ValidatedNel[JSONError, A] = fromJSON.read(jval)
      override val fields: Set[String] = fromJSON.fields
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
