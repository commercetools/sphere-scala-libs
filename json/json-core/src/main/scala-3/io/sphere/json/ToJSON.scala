package io.sphere.json

import io.sphere.json.generic.JSONTypeSwitch.ToFormatters
import org.json4s.JsonAST.JValue

/** Type class for types that can be written to JSON. */
trait ToJSON[A] extends Serializable {
  def write(value: A): JValue

  // Filled automatically for traits
  // I decided to not use option, because it's not an internal type anyway and
  // on traits it's always filled
  // on case classes it's always null
  // So there's not a lot of reasons to check for it runtime in most cases.
  val toFormatters: ToFormatters = null
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONInstances with ToJSONCatsInstances with generic.DeriveToJSON {
  inline def apply[A](implicit instance: ToJSON[A]): ToJSON[A] = instance
  inline def apply[A: JSON]: ToJSON[A] = summon[ToJSON[A]]

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue, toFs: ToFormatters = null): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)

    override val toFormatters: ToFormatters = toFs
  }
}
