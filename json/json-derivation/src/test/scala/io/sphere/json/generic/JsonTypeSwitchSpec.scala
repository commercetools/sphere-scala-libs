package io.sphere.json.generic

import io.sphere.json.JSON
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonTypeSwitchSpec extends AnyWordSpec with Matchers with JsonTypeSwitchSpecCommon {
  import JsonTypeSwitchModels._

  "jsonTypeSwitch" must {

    "derive a subset of a sealed trait" in {
      val format: JSON[A] = jsonTypeSwitch[A](List(sub[B], sub[C]))
      testDeriveASubsetOfASealedTrait(format)
    }

    "return an invalid result on malformed sum-type JSON" in {
      val format: JSON[A] = jsonTypeSwitch[A](List(sub[B], sub[C]))
      testMalformedSumTypeJson(format)
    }

    "derive a subset of a sealed trait with a mongoKey" in {
      val format: JSON[A] = jsonTypeSwitch[A](List(sub[B], sub[D]))
      testDeriveSubsetWithMongoKey(format)
    }

    "combine different sum types tree" in {
      val format: JSON[Message] = jsonTypeSwitch[Message](List(sub[TypeA], sub[TypeB]))
      testCombineSumTypes(format)
    }

    "handle custom implementations for subtypes" in {
      implicit val jsonB: JSON[B] = customJsonB
      val format: JSON[A] = jsonTypeSwitch[A](List(sub[B], sub[D], sub[C]))
      testCustomSubtypeImpl(format)
    }

    "handle the PlatformFormattedNotification case" in {
      testPlatformFormattedNotificationCase()
    }
  }
}
