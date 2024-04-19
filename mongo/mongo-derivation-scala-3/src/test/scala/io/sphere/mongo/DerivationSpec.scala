package io.sphere.mongo

import io.sphere.mongo.generic.TypedMongoFormat
import io.sphere.mongo.generic.TypedMongoFormat.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class DerivationSpec extends AnyWordSpec with Matchers:

  "MongoFormat derivation" should {
    "support composition" in {
      case class Container(i: Int, str: String, component: Component)
      case class Component(i: Int)

      val format = TypedMongoFormat[Container]

      val container = Container(123, "anything", Component(456))
      val bson = format.toMongoValue(container)
      val roundtrip = format.fromMongoValue(bson)

      // println(bson)
      // println(roundtrip)
      roundtrip mustBe container
    }

    "support ADT" in {
      sealed trait Root
      case object Object1 extends Root
      case object Object2 extends Root
      case class Class(i: Int) extends Root

      val format = TypedMongoFormat[Root]

      def roundtrip(member: Root): Unit =
        val bson = format.toMongoValue(member)
        val roundtrip = format.fromMongoValue(bson)

        // println(member)
        // println(bson)
        // println(roundtrip)
        roundtrip mustBe member

      roundtrip(Object1)
      roundtrip(Object2)
      roundtrip(Class(0))
    }
  }
