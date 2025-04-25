package io.sphere.json

import cats.implicits.*
import org.json4s.JsonAST.JValue

import scala.deriving.Mirror

trait JSON[A] extends FromJSON[A] with ToJSON[A]

inline def deriveJSON[A](using Mirror.Of[A]): JSON[A] = JSON.derived

object JSON extends JSONCatsInstances {
  inline def apply[A: JSON]: JSON[A] = summon[JSON[A]]
  inline given derived[A](using fromJSON: FromJSON[A], toJSON: ToJSON[A]): JSON[A] = instance

  def instance[A](
      readFn: JValue => JValidation[A],
      writeFn: A => JValue,
      fieldSet: Set[String] = FromJSON.emptyFieldsSet): JSON[A] = new {

    override def read(jval: JValue): JValidation[A] = readFn(jval)
    override def write(value: A): JValue = writeFn(value)
    override val fields: Set[String] = fieldSet
  }

  private def instance[A](using fromJSON: FromJSON[A], toJSON: ToJSON[A]): JSON[A] =
    new JSON[A] {
      override def read(jval: JValue): JValidation[A] = fromJSON.read(jval)

      override def write(value: A): JValue = toJSON.write(value)

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
