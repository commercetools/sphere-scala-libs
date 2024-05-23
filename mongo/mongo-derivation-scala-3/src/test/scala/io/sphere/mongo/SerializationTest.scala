package io.sphere.mongo

import com.mongodb.BasicDBObject
import io.sphere.mongo.generic.{DefaultMongoFormats, TypedMongoFormat}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

object SerializationTest:
  // For semi-automatic derivarion
  case class Something(a: Option[Int], b: Int = 2)

  // For Automatic derivation with `derives`
  case class Frunfles(a: Option[Int], b: Int = 2) derives TypedMongoFormat

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

      // Using backwards-compatible `deriveMongoFormat` + `implicit`
      implicit val x: TypedMongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat

      val something = TypedMongoFormat[Something].fromMongoValue(dbo)
      something mustBe Something(Some(3), 4)
    }

    "generate a format that serializes optional fields with value None as BSON objects without that field" in {
      // Using new Scala 3 `derived` special method + `given`
      given TypedMongoFormat[Something] = TypedMongoFormat.derived

      val something = Something(None, 1)
      val serializedObject =
        TypedMongoFormat[Something].toMongoValue(something).asInstanceOf[BasicDBObject]

      serializedObject.keySet().contains("b") must be(true)
      serializedObject.keySet().contains("a") must be(false)
      TypedMongoFormat[Something].fromMongoValue(serializedObject) must be(something)
    }

    "generate a format that serializes optional fields with value None as BSON objects without that field (using derives)" in {
      // Using an automatically-derived type via new Scala 3 `derives` directive
      val frunfles = Frunfles(None, 1)

      val serializedObject =
        TypedMongoFormat[Frunfles].toMongoValue(frunfles).asInstanceOf[BasicDBObject]

      serializedObject.keySet().contains("b") must be(true)
      serializedObject.keySet().contains("a") must be(false)
      TypedMongoFormat[Frunfles].fromMongoValue(serializedObject) must be(frunfles)
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
