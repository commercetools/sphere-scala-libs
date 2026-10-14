package io.sphere.json

import org.json4s.JsonAST._

import scala.annotation.implicitNotFound

/** Type class for types that can be written to JSON. */
@implicitNotFound("Could not find an instance of ToJSON for ${A}")
trait ToJSON[@specialized A] extends Serializable {
  def write(value: A): JValue
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONCatsInstances with ToJSONInstances {

  @inline def apply[A](implicit instance: ToJSON[A]): ToJSON[A] = instance

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)
  }
}
