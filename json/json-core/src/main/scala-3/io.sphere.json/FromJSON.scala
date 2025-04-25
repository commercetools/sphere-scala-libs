package io.sphere.json

import io.sphere.json.JValidation
import org.json4s.JsonAST.JValue

/** Type class for types that can be read from JSON. */
trait FromJSON[A] extends Serializable {
  def read(jval: JValue): JValidation[A]
  final protected def fail(msg: String) = jsonParseError(msg)

  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = FromJSON.emptyFieldsSet

  /** This is used in the TypeSwitch for cases when there's a nested trait with some type hints on
    * the case classes of the nested trait. We somehow need to know about the typehints, with this
    * we can propagate them.
    */
  val traitTypeHintMap: Map[String, String] = FromJSON.emptyTypeHintMap
}

object FromJSON extends FromJSONInstances with FromJSONCatsInstances with generic.DeriveFromJSON {
  val emptyFieldsSet: Set[String] = Set.empty
  val emptyTypeHintMap: Map[String, String] = Map.empty

  inline def apply[A](using instance: FromJSON[A]): FromJSON[A] = instance

  def instance[A](
      readFn: JValue => JValidation[A],
      fieldSet: Set[String] = emptyFieldsSet,
      typeHintMap: Map[String, String] = emptyTypeHintMap): FromJSON[A] = new {

    override def read(jval: JValue): JValidation[A] = readFn(jval)
    override val fields: Set[String] = fieldSet
    override val traitTypeHintMap: Map[String, String] = typeHintMap
  }
}
