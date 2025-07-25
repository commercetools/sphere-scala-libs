package io.sphere.json

import cats.implicits.*
import io.sphere.json.generic.JSONTypeSwitch.{FromFormatters, ToFormatters, fromJsonTypeSwitch}
import org.json4s.JsonAST.JValue

trait JSON[A] extends FromJSON[A] with ToJSON[A] {
  // This field is only used in case we derive a trait, for classes/objects it remains empty
  // It uses the JSON names not the Scala names (if there's @JSONTypeHint renaming a class the renamed name is used here)
  def subTypeNames: Vector[String] = Vector.empty
}

object JSON extends JSONCatsInstances {
  inline def apply[A: JSON]: JSON[A] = summon[JSON[A]]
  inline given derived[A](using fromJSON: FromJSON[A], toJSON: ToJSON[A]): JSON[A] =
    instance(
      readFn = fromJSON.read,
      writeFn = toJSON.write,
      fromFs = fromJSON.fromFormatters,
      toFs = toJSON.toFormatters,
      fieldSet = fromJSON.fields,
      subTypeNameList =
        Option(fromJSON.fromFormatters).map(_.serializedNames).getOrElse(Vector.empty)
    )

  def instance[A](
      readFn: JValue => JValidation[A],
      writeFn: A => JValue,
      fromFs: FromFormatters,
      toFs: ToFormatters,
      subTypeNameList: Vector[String] = Vector.empty,
      fieldSet: Set[String] = FromJSON.emptyFieldsSet): JSON[A] with TypeSelectorContainer =
    new JSON[A] with TypeSelectorContainer {
      override def read(jval: JValue): JValidation[A] = readFn(jval)
      override def write(value: A): JValue = writeFn(value)
      override val fields: Set[String] = fieldSet
      override def subTypeNames: Vector[String] = subTypeNameList
      override val fromFormatters: FromFormatters = fromFs
      override val toFormatters: ToFormatters = toFs

      override def typeSelectors: List[TypeSelector] = ???
    }
}

// Compatibility with Scala 2 syntax
// provide merging
case class TypeSelector(json: JSON[_]) {}
trait TypeSelectorContainer {
  def typeSelectors: List[TypeSelector]
}

class JSONException(msg: String) extends RuntimeException(msg)

sealed abstract class JSONError
case class JSONFieldError(path: List[String], message: String) extends JSONError {
  override def toString = path.mkString(" -> ") + ": " + message
}
case class JSONParseError(message: String) extends JSONError {
  override def toString = message
}
