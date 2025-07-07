package io.sphere.json

import io.sphere.json.JValidation
import io.sphere.json.generic.JSONTypeSwitch.Formatters
import org.json4s.JsonAST.JValue

/** Type class for types that can be read from JSON. */
trait FromJSON[A] extends Serializable {
  def read(jval: JValue): JValidation[A]
  final protected def fail(msg: String) = jsonParseError(msg)

  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = FromJSON.emptyFieldsSet

  // This is automatically filled for traits
  val fromFormatters: Formatters[FromJSON] = null
}

object FromJSON extends FromJSONInstances with FromJSONCatsInstances with generic.DeriveFromJSON {
  val emptyFieldsSet: Set[String] = Set.empty

  inline def apply[A](using instance: FromJSON[A]): FromJSON[A] = instance

  def instance[A](
      readFn: JValue => JValidation[A],
      fromFs: Formatters[FromJSON],
      fieldSet: Set[String] = emptyFieldsSet): FromJSON[A] = new {

    override def read(jval: JValue): JValidation[A] = readFn(jval)
    override val fields: Set[String] = fieldSet
    override val fromFormatters: Formatters[FromJSON] = fromFs
  }
}
