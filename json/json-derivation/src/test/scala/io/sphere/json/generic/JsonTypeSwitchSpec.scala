package io.sphere.json.generic

import io.sphere.json.JSON
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonTypeSwitchSpec extends AnyWordSpec with Matchers with JsonTypeSwitchSpecCommon {
  import JsonTypeSwitchModels._

  "jsonTypeSwitch" must {

    "derive a subset of a sealed trait" in {
      val format: JSON[A] = jsonTypeSwitch[A, B, C](Nil)
      testDeriveASubsetOfASealedTrait(format)
    }

    "derive a subset of a sealed trait with a mongoKey" in {
      val format: JSON[A] = jsonTypeSwitch[A, B, D](Nil)
      testDeriveSubsetWithMongoKey(format)
    }

    "combine different sum types tree" in {
      val format: JSON[Message] = jsonTypeSwitch[Message, TypeA, TypeB](Nil)
      testCombineSumTypes(format)
    }

    "handle custom implementations for subtypes" in {
      implicit val jsonB: JSON[B] = customJsonB
      val format: JSON[A] = jsonTypeSwitch[A, B, D, C](Nil)
      testCustomSubtypeImpl(format)
    }

    "handle the PlatformFormattedNotification case" in {
      testPlatformFormattedNotificationCase()
    }
  }
}
