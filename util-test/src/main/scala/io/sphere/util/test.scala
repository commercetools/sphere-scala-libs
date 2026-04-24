package io.sphere.util

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import org.scalatest.Assertions._

package object test {
  implicit class ValidatedNelTestOps[Err, A](val validatedNel: ValidatedNel[Err, A])
      extends AnyVal {
    def expectValid: A = validatedNel match {
      case Valid(a) => a
      case Invalid(e) => fail(s"Expected Valid, but got: $e")
    }

    def expectErrors: List[Err] = validatedNel match {
      case Invalid(e) => e.toList
      case Valid(a) => fail(s"Expected Invalid, but got: $a")
    }

    def expectError: Err = validatedNel match {
      case Invalid(cats.data.NonEmptyList(head, Nil)) => head
      case Valid(a) => fail(s"Expected Invalid, but got: $a")
      case Invalid(e) =>
        val formattedErr = e.toList.mkString("[", ", ", "]")
        fail(s"Expected a single error, but got: $formattedErr")
    }
  }
}
