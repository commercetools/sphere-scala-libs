package io.sphere.json

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BigNumberParsingSpec extends AnyWordSpec with Matchers {
  import BigNumberParsingSpec._

  "parsing a big number" should {
    "not take much time when parsed as Double" in {
      fromJSON[Double](bigNumberAsString).isValid should be(false)
    }
    "not take much time when parsed as Long" in {
      fromJSON[Long](bigNumberAsString).isValid should be(false)
    }
    "not take much time when parsed as Int" in {
      fromJSON[Int](bigNumberAsString).isValid should be(false)
    }
  }
}

object BigNumberParsingSpec {
  private val bigNumberAsString = "9" * 10000000
}
