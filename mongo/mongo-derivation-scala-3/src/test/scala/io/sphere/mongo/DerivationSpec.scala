package io.sphere.mongo

import io.sphere.mongo.generic.{FakeBson, FakeMongoFormat, SingleValue}
import io.sphere.mongo.generic.FakeMongoFormat.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class DerivationSpec extends AnyWordSpec with Matchers:

  "MongoFormat derivation" should {
    "support composition" in {
      case class Container(i: Int, str: String, component: Component)
      case class Component(i: Int)

      val format = FakeMongoFormat[Container]

      val container = Container(123, "anything", Component(456))
      val bson = format.toFakeBson(container)
      val roundtrip = format.fromFakeBson(bson)

      // println(bson)
      // println(roundtrip)
      roundtrip mustBe container
    }

    "support ADT" in {
      sealed trait Root
      case object Object1 extends Root
      case object Object2 extends Root
      case class Class(i: Int) extends Root

      val format = FakeMongoFormat[Root]

      def roundtrip(member: Root): Unit =
        val bson = format.toFakeBson(member)
        val roundtrip = format.fromFakeBson(bson)

        // println(member)
        // println(bson)
        // println(roundtrip)
        roundtrip mustBe member

      roundtrip(Object1)
      roundtrip(Object2)
      roundtrip(Class(0))
    }
  }
