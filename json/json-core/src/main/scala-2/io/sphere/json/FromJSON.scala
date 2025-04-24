package io.sphere.json

import org.json4s.JsonAST._

import scala.annotation.implicitNotFound

/** Type class for types that can be read from JSON. */
@implicitNotFound("Could not find an instance of FromJSON for ${A}")
trait FromJSON[@specialized A] extends Serializable {
  def read(jval: JValue): JValidation[A]
  final protected def fail(msg: String) = jsonParseError(msg)

  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = FromJSON.emptyFieldsSet
}

object FromJSON extends FromJSONInstances with FromJSONCatsInstances {

  private[FromJSON] val emptyFieldsSet: Set[String] = Set.empty

  @inline def apply[A](implicit instance: FromJSON[A]): FromJSON[A] = instance

}
