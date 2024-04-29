package io.sphere.mongo.format

import io.sphere.mongo.generic.{MongoEmbedded, MongoKey, MongoTypeHintField, TypedMongoFormat}
import io.sphere.mongo.generic.TypedMongoFormat.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class DerivationSpec extends AnyWordSpec with Matchers:

  "MongoFormat derivation" should {
    "support composition" in {
      case class Container(i: Int, str: String, component: Component)
      case class Component(i: Int)

      val format = io.sphere.mongo.generic.deriveMongoFormat[Container]

      val container = Container(123, "anything", Component(456))
      val bson = format.toMongoValue(container)
      val roundtrip = format.fromMongoValue(bson)

      roundtrip mustBe container
    }

    "support ADT" in {
      sealed trait Root
      case object Object1 extends Root
      case object Object2 extends Root
      case class Class(i: Int) extends Root

      val format = io.sphere.mongo.generic.deriveMongoFormat[Root]

      def roundtrip(member: Root): Unit =
        val bson = format.toMongoValue(member)
        val roundtrip = format.fromMongoValue(bson)
        roundtrip mustBe member

      roundtrip(Object1)
      roundtrip(Object2)
      roundtrip(Class(0))

    }

    "annotations" in {

      case class InnerClass(x: String)
      @MongoTypeHintField("pictureType")
      sealed trait Root
      case object Object1 extends Root
      case object Object2 extends Root
      case class Class(i: Int, @MongoEmbedded inner: InnerClass) extends Root

      val res = readCaseClassMetaData[Root]

    }
  }
