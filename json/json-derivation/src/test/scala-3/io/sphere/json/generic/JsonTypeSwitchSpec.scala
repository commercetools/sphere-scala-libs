package io.sphere.json.generic

import cats.implicits._
import io.sphere.json.JSON
import io.sphere.util.test._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonTypeSwitchSpec extends AnyWordSpec with Matchers with JsonTypeSwitchBehaviors {
  import JsonTypeSwitchModels._

  "jsonTypeSwitch" must {

    {
      given JSON[B] = deriveJSON[B]

      "derive a subset of a sealed trait (old syntax)" in {
        testDeriveASubsetOfASealedTrait(jsonTypeSwitch[A, B, C](Nil))
      }
      "derive a subset of a sealed trait (new syntax)" in {
        testDeriveASubsetOfASealedTrait(jsonTypeSwitch[A, (B, C)])
      }
    }

    "derive a subset of a sealed trait with a mongoKey (old syntax)" in {
      testDeriveSubsetWithMongoKey(jsonTypeSwitch[A, B, D](Nil))
    }
    "derive a subset of a sealed trait with a mongoKey (new syntax)" in {
      testDeriveSubsetWithMongoKey(jsonTypeSwitch[A, (B, D)])
    }

    "combine different sum types tree (old syntax)" in {
      testCombineSumTypes(jsonTypeSwitch[Message, TypeA, TypeB](Nil))
    }
    "combine different sum types tree (new syntax)" in {
      testCombineSumTypes(jsonTypeSwitch[Message, (TypeA, TypeB)])
    }

    {
      given JSON[B] = customJsonB

      "handle custom implementations for subtypes (old syntax)" in {
        testCustomSubtypeImpl(jsonTypeSwitch[A, B, D, C](Nil))
      }
      "handle custom implementations for subtypes (new syntax)" in {
        testCustomSubtypeImpl(jsonTypeSwitch[A, (B, D, C)])
      }
    }

    "handle the PlatformFormattedNotification case" when {
      "using the /old/ syntax" in {
        testPlatformFormattedNotificationCase()
      }

      "using the /new/ syntax" in {
        type Trait234 = SubTrait2 *: (SubTrait3, SubTrait4)

        val formatSuper: JSON[SuperTrait] = jsonTypeSwitch[SuperTrait, SubTrait1 *: Trait234]

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
