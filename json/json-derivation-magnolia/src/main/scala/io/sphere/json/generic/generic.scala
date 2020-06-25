package io.sphere.json

import cats.data.ValidatedNel
import io.sphere.json.generic.derive.{FromJSONDerivation, ToJSONDerivation}
import magnolia.{CaseClass, Magnolia, SealedTrait}
import org.json4s.JsonAST.{JString, JValue}
import cats.syntax.option._

import scala.language.experimental.macros

package object generic {

  type Typeclass[T] = JSON[T]

  def deriveJSON[T]: JSON[T] = macro Magnolia.gen[T]
  def jsonProduct[T]: JSON[T] = macro Magnolia.gen[T]

  /** Creates a ToJSON instance for an Enumeration type that encodes the `toString`
   * representations of the enumeration values. */
  def toJsonEnum(e: Enumeration): ToJSON[e.Value] = new ToJSON[e.Value] {
    def write(a: e.Value): JValue = JString(a.toString)
  }

  /** Creates a FromJSON instance for an Enumeration type that encodes the `toString`
   * representations of the enumeration values. */
  def fromJsonEnum(e: Enumeration): FromJSON[e.Value] = new FromJSON[e.Value] {
    def read(jval: JValue): ValidatedNel[JSONError, e.Value] = jval match {
      case JString(s) => e.values.find(_.toString == s).toValidNel(
        JSONParseError("Invalid enum value: '%s'. Expected one of: %s".format(s, e.values.mkString("','")))
      )
      case _ => jsonParseError("JSON String expected.")
    }
  }

  def combine[T <: Product](caseClass: CaseClass[JSON, T]): JSON[T] =
    new JSON[T] {
      private val to: ToJSON[T] = ToJSONDerivation.combine(
        caseClass.asInstanceOf[CaseClass[ToJSON, T]]
      )
      private val from: FromJSON[T] = FromJSONDerivation.combine(
        caseClass.asInstanceOf[CaseClass[FromJSON, T]]
      )

      override def write(r: T): JValue = to.write(r)
      override def read(jval: JValue): JValidation[T] = from.read(jval)
      override val fields: Set[String] = from.fields
    }

  def dispatch[T](sealedTrait: SealedTrait[JSON, T]): JSON[T] = new JSON[T] {
    private val to: ToJSON[T] = ToJSONDerivation.dispatch(
      sealedTrait.asInstanceOf[SealedTrait[ToJSON, T]]
    )
    private val from: FromJSON[T] = FromJSONDerivation.dispatch(
      sealedTrait.asInstanceOf[SealedTrait[FromJSON, T]]
    )
    override def write(t: T): JValue = to.write(t)
    override def read(jval: JValue): JValidation[T] = from.read(jval)
  }
}
