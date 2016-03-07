import _root_.io.sphere.util.Money
import _root_.io.sphere.json._

import org.json4s.jackson.compactJson
import org.scalatest.{Matchers, WordSpec}

import scalaz.Success

class MoneySymmetrySpec extends WordSpec with Matchers {
  "money encoding/decoding" should {
    "be symetric" in {
      val money = Money.EUR(34.56)
      val jsonAst = toJValue(money)
      val jsonAsString = compactJson(jsonAst)
      val Success(readAst) = parseJSON(jsonAsString)

      jsonAst should equal (readAst)
    }
  }

}