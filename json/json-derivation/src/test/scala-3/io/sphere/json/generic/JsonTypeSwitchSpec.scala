package io.sphere.json.generic

import cats.implicits._
import io.sphere.json.JSON
import io.sphere.util.test._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonTypeSwitchSpec extends AnyWordSpec with Matchers with JsonTypeSwitchSpecCommon {
  import JsonTypeSwitchModels._

  "jsonTypeSwitch" must {

    {
      given JSON[B] = deriveJSON[B]

      "derive a subset of a sealed trait" in {
        testDeriveASubsetOfASealedTrait(jsonTypeSwitch[A](List(sub[B], sub[C])))
      }
      "return an invalid result on malformed sum-type JSON" in {
        testMalformedSumTypeJson(jsonTypeSwitch[A](List(sub[B], sub[C])))
      }
    }

    "derive a subset of a sealed trait with a mongoKey" in {
      testDeriveSubsetWithMongoKey(jsonTypeSwitch[A](List(sub[B], sub[D])))
    }

    "combine different sum types tree" in {
      testCombineSumTypes(jsonTypeSwitch[Message](List(sub[TypeA], sub[TypeB])))
    }

    {
      given JSON[B] = customJsonB

      "handle custom implementations for subtypes" in {
        testCustomSubtypeImpl(jsonTypeSwitch[A](List(sub[B], sub[D], sub[C])))
      }
    }

    "handle the PlatformFormattedNotification case" when {
      "merging the sub-switches' selectors" in {
        testPlatformFormattedNotificationCase()
      }

      "listing the sub-traits directly" in {
        val formatSuper: JSON[SuperTrait] =
          jsonTypeSwitch[SuperTrait](
            List(sub[SubTrait1], sub[SubTrait2], sub[SubTrait3], sub[SubTrait4]))

        val objs =
          List[SuperTrait](
            SubTrait1.O1,
            SubTrait1.O2,
            SubTrait2.O3,
            SubTrait2.O4,
            SubTrait3.O5,
            SubTrait3.O6,
            SubTrait4.O7)

        val res = objs.map(formatSuper.write).traverse(formatSuper.read).expectValid
        res must be(objs)
      }
    }
  }
}
