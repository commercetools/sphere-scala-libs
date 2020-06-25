package io.sphere.json.generic.derive

import cats.data.Validated.Valid
import io.sphere.json.generic.derive.CommonDerivation.jsonClassMetaFromCaseClass
import io.sphere.json.{FromJSON, JSON, JValidation, ToJSON, jsonParseError}
import magnolia.{CaseClass, Magnolia, SealedTrait}
import org.json4s.JsonAST.{JString, JValue}

import scala.language.experimental.macros

object SingletonJSONDerivation {
  type Typeclass[T] = JSON[T]

  def deriveSingletonJSON[T]: JSON[T] = macro Magnolia.gen[T]

  def combine[T <: Product](caseClass: CaseClass[JSON, T]): JSON[T] = {
    if (!caseClass.isObject)
      throw new Exception(s"can only be used on singleton and not on '${caseClass.typeName.full}'")
    val jsonClass = jsonClassMetaFromCaseClass(caseClass.asInstanceOf[CaseClass[CommonDerivation.Typeclass, T]])
    new JSON[T] {
      override def write(r: T): JValue =
        jsonClass.typeHint match {
          case Some(th) => JString(th.value)
          case None => throw new Exception("Singleton '${caseClass.typeName.full}' does not have any type hint.")
        }
      override def read(jval: JValue): JValidation[T] =
        jval match {
          case JString(s) => Valid(caseClass.rawConstruct(Seq.empty))
          case _ => jsonParseError("JSON string expected.")
        }
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[JSON, T]): JSON[T] = new JSON[T] {
    private val to: ToJSON[T] = ToJSONDerivation.dispatchInternal(
      sealedTrait.asInstanceOf[SealedTrait[ToJSON, T]],
      compactCaseObject = true
    )
    private val from: FromJSON[T] = FromJSONDerivation.dispatchInternal(
      sealedTrait.asInstanceOf[SealedTrait[FromJSON, T]],
      compactCaseObject = true
    )
    override def write(t: T): JValue = to.write(t)
    override def read(jval: JValue): JValidation[T] = from.read(jval)
  }


}
