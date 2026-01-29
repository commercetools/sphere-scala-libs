package io.sphere.json

import io.sphere.json.generic._
import org.json4s.JsonAST.{JNothing, JObject}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import io.sphere.util.BaseMoney
import io.sphere.util.Money.ImplicitsDecimal._

import java.util.UUID

class NullHandlingSpec extends AnyWordSpec with Matchers {
  "JSON deserialization" must {
    "accept undefined fields and use default values for them" in {
      val jeans = getFromJSON[Jeans]("{}")

      jeans must be(Jeans(None, None, Set.empty, "secret"))
    }

    "accept null values and use default values for them" in {
      val jeans = getFromJSON[Jeans]("""
          {
            "leftPocket": null,
            "rightPocket": null,
            "backPocket": null,
            "hiddenPocket": null
          }
        """)

      jeans must be(Jeans(None, None, Set.empty, "secret"))
    }

    "accept JNothing values and use default values for them" in {
      val jeans = getFromJValue[Jeans](
        JObject(
          "leftPocket" -> JNothing,
          "rightPocket" -> JNothing,
          "backPocket" -> JNothing,
          "hiddenPocket" -> JNothing))

      jeans must be(Jeans(None, None, Set.empty, "secret"))
    }

    "accept not-null values and use them" in {
      val jeans = getFromJSON[Jeans]("""
          {
            "leftPocket": "Axe",
            "rightPocket": "Magic powder",
            "backPocket": ["Magic wand", "Rusty sword"],
            "hiddenPocket": "The potion of healing"
          }
        """)

      jeans must be(
        Jeans(
          Some("Axe"),
          Some("Magic powder"),
          Set("Magic wand", "Rusty sword"),
          "The potion of healing"))
    }

    "Use nested FromJSON instances" in {

      // This is relevant because it can fail if the implicits are not in the right scope
      // If it fails it'll happily autoderive the inner trait (BaseMoney), which leads to successful compilation but incorrect JSON
      val action = MoneyOptClass(Some(10.EUR))

      val format: JSON[MoneyOptClass] = deriveJSON
      val json = format.write(action)
      val action2 = format.read(json).getOrElse(null)

      action must be(action2)
    }
  }

}
//
case class MoneyOptClass(moneyOpt: Option[BaseMoney])

case class Jeans(
    leftPocket: Option[String] = None,
    rightPocket: Option[String],
    backPocket: Set[String] = Set.empty,
    hiddenPocket: String = "secret")

object Jeans {
  implicit val json: JSON[Jeans] = deriveJSON[Jeans]
}
