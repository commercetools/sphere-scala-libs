package io.sphere.json

import scala.annotation.implicitNotFound
import java.time
import org.json4s.JsonAST.JValue

/** Type class for types that can be written to JSON. */
@implicitNotFound("Could not find an instance of ToJSON for ${A}")
trait ToJSON[@specialized A] extends Serializable {
  def write(value: A): JValue
}

class JSONWriteException(msg: String) extends JSONException(msg)

object ToJSON extends ToJSONInstances with ToJSONCatsInstances {

  @inline def apply[A](implicit instance: ToJSON[A]): ToJSON[A] = instance

  /** construct an instance from a function
    */
  def instance[T](toJson: T => JValue): ToJSON[T] = new ToJSON[T] {
    override def write(value: T): JValue = toJson(value)
  }

}
