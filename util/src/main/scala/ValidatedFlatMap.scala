package io.sphere.util

import cats.data.Validated

class ValidatedFlatMap[E, A](val v: Validated[E, A]) extends AnyVal {
  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    v.andThen(f)
}

/** Cats [[Validated]] does not provide `flatMap` because its purpose is
  * to accumulate errors.
  *
  * To combine [[Validated]] in for-comprehension, it is possible to import
  * this implicit conversion - with the knowledge that the `flatMap`
  * short-circuits errors.
  * http://typelevel.org/cats/datatypes/validated.html
  */
object ValidatedFlatMapFeature {
  import scala.language.implicitConversions

  @inline implicit def ValidationFlatMapRequested[E, A](
      d: Validated[E, A]): ValidatedFlatMap[E, A] =
    new ValidatedFlatMap(d)

}
