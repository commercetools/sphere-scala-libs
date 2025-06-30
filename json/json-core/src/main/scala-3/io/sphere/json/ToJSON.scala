package io.sphere.json

import io.sphere.json.generic.JSONTypeSwitch.Formatters
import org.json4s.JsonAST.JValue

/** Type class for types that can be written to JSON. */
trait ToJSON[A] extends Serializable {
  def write(value: A): JValue

  // Filled automatically for traits
  val toFormatters: Formatters[ToJSON] = null
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONInstances with ToJSONCatsInstances with generic.DeriveToJSON {
  inline def apply[A](implicit instance: ToJSON[A]): ToJSON[A] = instance
  inline def apply[A: JSON]: ToJSON[A] = summon[ToJSON[A]]

  /** construct an instance from a function
    */
  def instance[T](toFs: Formatters[ToJSON])(toJson: T => JValue): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)
    override val toFormatters: Formatters[ToJSON] = toFs
  }
}
