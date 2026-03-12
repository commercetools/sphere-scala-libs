package io.sphere.util

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel

// These should only be used in tests
package object test {
  implicit class ValidatedNelTestOps[Err, A](val validatedNel: ValidatedNel[Err, A])
      extends AnyVal {
    def expectValid: A = validatedNel match {
      case Valid(a) => a
      case Invalid(e) => throw new IllegalStateException(s"Expected Valid, but got: $e")
    }

    def expectErrors: List[Err] = validatedNel match {
      case Invalid(e) => e.toList
      case Valid(a) => throw new IllegalStateException(s"Expected Invalid, but got: $a")
    }

    def expectError: Err = validatedNel match {
      case Invalid(cats.data.NonEmptyList(head, Nil)) => head
      case Valid(a) => throw new IllegalStateException(s"Expected Invalid, but got: $a")
      case Invalid(e) =>
        val formattedErr = e.toList.mkString("[", ", ", "]")
        throw new IllegalStateException(s"Expected a single error, but got: $formattedErr")
    }
  }
}
