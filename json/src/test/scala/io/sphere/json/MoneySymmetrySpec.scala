package io.sphere.json

import io.sphere.util.Money
import cats.data.Validated.Valid
import org.json4s.jackson.compactJson
import org.scalatest.{Matchers, WordSpec}

class MoneySymmetrySpec extends WordSpec with Matchers {
  "money encoding/decoding" should {
    "be symetric" in {
      val money = Money.EUR(34.56)
      val jsonAst = toJValue(money)
      val jsonAsString = compactJson(jsonAst)
      val Valid(readAst) = parseJSON(jsonAsString)

      jsonAst should equal (readAst)
    }
  }

}
