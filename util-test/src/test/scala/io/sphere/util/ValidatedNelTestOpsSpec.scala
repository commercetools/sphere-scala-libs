package io.sphere.util

import cats.data.Validated.{Invalid, Valid}
import cats.data.NonEmptyList
import io.sphere.util.test._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class ValidatedNelTestOpsSpec extends AnyFunSpec with Matchers {

  describe("expectValid") {
    it("should return the value when Valid") {
      val result = Valid(42)
      result.expectValid must be(42)
    }

    it("should throw TestFailedException when Invalid") {
      val result = Invalid(NonEmptyList.one("error"))
      val ex = the[TestFailedException] thrownBy result.expectValid
      ex.getMessage must include("Expected Valid")
      ex.getMessage must include("error")
    }
  }

  describe("expectErrors") {
    it("should return the list of errors when Invalid with one error") {
      val ex = new Exception("err1")
      val result = Invalid(NonEmptyList.one(ex))

      result.expectErrors must be(List(ex))
    }

    it("should return the list of errors when Invalid with multiple errors") {
      val result = Invalid(NonEmptyList.of("err1", "err2", "err3"))
      result.expectErrors must be(List("err1", "err2", "err3"))
    }

    it("should throw TestFailedException when Valid") {
      val result = Valid(42)
      val ex = the[TestFailedException] thrownBy result.expectErrors
      ex.getMessage must be("Expected Invalid, but got: 42")
    }
  }

  describe("expectError") {
    it("should return the single error when Invalid with exactly one error") {
      val result = Invalid(NonEmptyList.one("only-error"))
      result.expectError must be("only-error")
    }

    it("should throw TestFailedException when Valid") {
      val result = Valid(42)
      val ex = the[TestFailedException] thrownBy result.expectError
      ex.getMessage must be("Expected Invalid, but got: 42")
    }

    it("should throw IllegalStateException when Invalid with multiple errors") {
      val result = Invalid(NonEmptyList.of(new Exception("err1"), new Exception("err2")))
      val ex = the[TestFailedException] thrownBy result.expectError
      ex.getMessage must be(
        "Expected a single error, but got: [java.lang.Exception: err1, java.lang.Exception: err2]")
    }
  }
}
