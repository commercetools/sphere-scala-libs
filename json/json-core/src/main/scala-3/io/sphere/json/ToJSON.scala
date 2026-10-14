package io.sphere.json

import io.sphere.json.generic.JSONTypeSwitch.ToFormatters
import org.json4s.JsonAST.*

import scala.annotation.implicitNotFound

/** Type class for types that can be written to JSON. */
@implicitNotFound("Could not find an instance of ToJSON for ${A}")
trait ToJSON[A] extends Serializable {
  def write(value: A): JValue

  // Filled automatically for traits
  // I decided to not use option, because it's not an internal type anyway and
  // on traits it's always filled
  // on case classes it's always null
  // So there's not a lot of reasons to check for it runtime in most cases.
  val toFormatters: ToFormatters = null
}

object ToJSON extends ToJSONCatsInstances with ToJSONInstances with generic.DeriveToJSON {

  inline def apply[A](using instance: ToJSON[A]): ToJSON[A] = instance

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue, toFs: ToFormatters = null): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)

    override val toFormatters: ToFormatters = toFs
  }
}
