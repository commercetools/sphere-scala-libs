package io.sphere.json

import org.json4s.JsonAST.JValue

import java.time
import java.util.{Currency, Locale, UUID}

/** Type class for types that can be written to JSON. */
trait ToJSON[A] extends Serializable {
  def write(value: A): JValue
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONInstances with ToJSONCatsInstances with generic.DeriveToJSON {

  inline def apply[A](implicit instance: ToJSON[A]): ToJSON[A] = instance
  inline def apply[A: JSON]: ToJSON[A] = summon[ToJSON[A]]

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)
  }
}
