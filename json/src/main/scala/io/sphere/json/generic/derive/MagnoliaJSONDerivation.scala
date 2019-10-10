package io.sphere.json.generic.derive

import io.sphere.json.{FromJSON, JSON, JValidation, ToJSON}
import magnolia._
import org.json4s.JsonAST.JValue

import scala.language.experimental.macros

object MagnoliaJSONDerivation {

  type Typeclass[T] = JSON[T]

  def deriveJSON[T]: JSON[T] = macro Magnolia.gen[T]
  def jsonProduct[T]: JSON[T] = macro Magnolia.gen[T]

  def combine[T <: Product](caseClass: CaseClass[JSON, T]): JSON[T] = new JSON[T] {
    private val to: ToJSON[T] = MagnoliaToJSONDerivation.combine(caseClass.asInstanceOf[CaseClass[ToJSON, T]])
    private val from: FromJSON[T] = MagnoliaFromJSONDerivation.combine(caseClass.asInstanceOf[CaseClass[FromJSON, T]])

    override def write(r: T): JValue = to.write(r)
    override def read(jval: JValue): JValidation[T] = from.read(jval)
    override val fields: Set[String] = from.fields
  }

  def dispatch[T](sealedTrait: SealedTrait[JSON, T]): JSON[T] = new JSON[T] {
    private val to: ToJSON[T] = MagnoliaToJSONDerivation.dispatch(sealedTrait.asInstanceOf[SealedTrait[ToJSON, T]])
    private val from: FromJSON[T] = MagnoliaFromJSONDerivation.dispatch(sealedTrait.asInstanceOf[SealedTrait[FromJSON, T]])
    override def write(t: T): JValue = to.write(t)
    override def read(jval: JValue): JValidation[T] = from.read(jval)
  }
}
