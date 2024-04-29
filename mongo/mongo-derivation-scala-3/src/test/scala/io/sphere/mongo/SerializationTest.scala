package io.sphere.mongo

import com.mongodb.BasicDBObject
import io.sphere.mongo.generic.TypedMongoFormat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

object SerializationTest:
  case class Something(a: Option[Int], b: Int = 2)

  object Color extends Enumeration:
    val Blue, Red, Yellow = Value

  sealed trait PictureSize
  case object Small extends PictureSize
  case object Medium extends PictureSize

class SerializationTest extends AnyWordSpec with Matchers:
  import SerializationTest.*

  "mongoProduct" must {
    "deserialize mongo object" in {
      val dbo = new BasicDBObject
      dbo.put("a", Integer.valueOf(3))
      dbo.put("b", Integer.valueOf(4))

      // TODO what is this? :D
      val med: TypedMongoFormat[Medium.type] = io.sphere.mongo.generic.deriveMongoFormat

      val mongoFormat: TypedMongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat
      val something = mongoFormat.fromMongoValue(dbo)
      something mustBe Something(Some(3), 4)
    }

    "generate a format that serializes optional fields with value None as BSON objects without that field" in {
      val testFormat: TypedMongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat

      val something = Something(None, 1)
      val serializedObject = testFormat.toMongoValue(something).asInstanceOf[BasicDBObject]
      serializedObject.keySet().contains("b") must be(true)
      serializedObject.keySet().contains("a") must be(false)

      testFormat.fromMongoValue(serializedObject) must be(something)
    }

//    "generate a format that use default values" in {
//      // TODO https://stackoverflow.com/questions/68421043/type-class-derivation-accessing-default-values
//      val dbo = new BasicDBObject()
//      dbo.put("a", Integer.valueOf(3))
//
//      val mongoFormat: TypedMongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat
//      val something = mongoFormat.fromMongoValue(dbo)
//      something must be(Something(Some(3), 2))
//    }
  }

  "mongoEnum" must {
//    "serialize and deserialize enums" in {
//      val mongo: TypedMongoFormat[Color.Value] = io.sphere.mongo.generic.deriveMongoFormat
//
//      // mongo java driver knows how to encode/decode Strings
//      val serializedObject = mongo.toMongoValue(Color.Red).asInstanceOf[String]
//      serializedObject must be("Red")
//
//      val enumValue = mongo.fromMongoValue(serializedObject)
//      enumValue must be(Color.Red)
//    }
  }
