package io.sphere.json

import io.sphere.json.generic.JSONTypeSwitch.FromFormatters
import org.json4s.JsonAST.*

import scala.annotation.implicitNotFound

/** Type class for types that can be read from JSON. */
@implicitNotFound("Could not find an instance of FromJSON for ${A}")
trait FromJSON[A] extends Serializable {
  def read(jval: JValue): JValidation[A]
  final protected def fail(msg: String) = jsonParseError(msg)

  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = FromJSON.emptyFieldsSet

  // This is automatically filled for traits
  val fromFormatters: FromFormatters = null
  def getSerializedNames: Vector[String] =
    if (fromFormatters == null) Vector.empty
    else fromFormatters.serializedNames
}

object FromJSON extends FromJSONCatsInstances with FromJSONInstances with generic.DeriveFromJSON {

  inline def apply[A](using instance: FromJSON[A]): FromJSON[A] = instance

  def instance[A](
      readFn: JValue => JValidation[A],
      fromFs: FromFormatters,
      fieldSet: Set[String] = emptyFieldsSet): FromJSON[A] = new {

    override def read(jval: JValue): JValidation[A] = readFn(jval)
    override val fields: Set[String] = fieldSet
    override val fromFormatters: FromFormatters = fromFs
  }
}
