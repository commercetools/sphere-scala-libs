package io.sphere.json

import _root_.cats.{Contravariant, Functor, Invariant}
import org.json4s.JValue

/** Cats instances for [[JSON]], [[FromJSON]] and [[ToJSON]]
  */
package object catsinstances extends JSONInstances with FromJSONInstances with ToJSONInstances

trait JSONInstances {
  implicit val catsInvariantForJSON: Invariant[JSON] = new JSONInvariant
}

trait FromJSONInstances {
  implicit val catsFunctorForFromJSON: Functor[FromJSON] = new FromJSONFunctor
}

trait ToJSONInstances {
  implicit val catsContravariantForToJSON: Contravariant[ToJSON] = new ToJSONContravariant
}

class JSONInvariant extends Invariant[JSON] {
  override def imap[A, B](fa: JSON[A])(f: A => B)(g: B => A): JSON[B] = new JSON[B] {
    override def write(b: B): JValue = fa.write(g(b))
    override def read(jval: JValue): JValidation[B] = fa.read(jval).map(f)
    override val fields: Set[String] = fa.fields
  }
}

class FromJSONFunctor extends Functor[FromJSON] {
  override def map[A, B](fa: FromJSON[A])(f: A => B): FromJSON[B] = new FromJSON[B] {
    override def read(jval: JValue): JValidation[B] = fa.read(jval).map(f)
    override val fields: Set[String] = fa.fields
  }
}

class ToJSONContravariant extends Contravariant[ToJSON] {
  override def contramap[A, B](fa: ToJSON[A])(f: B => A): ToJSON[B] = new ToJSON[B] {
    override def write(b: B): JValue = fa.write(f(b))
  }
}
