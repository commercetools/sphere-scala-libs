package io.sphere.mongo

import com.mongodb.BasicDBObject
import io.sphere.mongo.generic.TypedMongoFormat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.util.UUID

object ProductTypes:
  // For semi-automatic derivarion + default value argument
  case class Something(a: Option[Int], b: Int = 2)

  // For Automatic derivation with `derives`
  case class Frunfles(a: Option[Int], b: Int) derives TypedMongoFormat

  // Union type field - doesn't compile!
  // case class Identifier(idOrKey: UUID | String) derives TypedMongoFormat
end ProductTypes

object SumTypes:
  object Color extends Enumeration:
    val Blue, Red, Yellow = Value

  sealed trait Coffee derives TypedMongoFormat
  object Coffee:
    case object Espresso extends Coffee
    case class Other(name: String) extends Coffee

  enum Visitor derives TypedMongoFormat:
    case User(email: String, password: String)
    case Anonymous
end SumTypes

class SerializationTest extends AnyWordSpec with Matchers:
  "mongoProduct" must {
    import ProductTypes.*

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

    "generate a format that use default values" in {
      // // TODO https://stackoverflow.com/questions/68421043/type-class-derivation-accessing-default-values
      // val dbo = new BasicDBObject()
      // dbo.put("a", Integer.valueOf(3))

      // val mongoFormat: TypedMongoFormat[Something] = io.sphere.mongo.generic.deriveMongoFormat
      // val something = mongoFormat.fromMongoValue(dbo)
      // something must be(Something(Some(3), 2))
    }
  }

  // Both sealed hierarchies and enums get this "type" field, even the Singleton cases.
  // Is this what we want, or should the Singleton cases be just a String?
  "mongoSum" must {
    import SumTypes.*

    "serialize and deserialize sealed hierarchies" in {
      val mongo = TypedMongoFormat[Coffee]

      val espressoObj = {
        val dbo = new BasicDBObject
        dbo.put("type", "Espresso")
        dbo
      }
      val serializedEspresso = mongo.toMongoValue(Coffee.Espresso)
      val deserializedEspresso = mongo.fromMongoValue(serializedEspresso)
      serializedEspresso must be(espressoObj)
      deserializedEspresso must be(Coffee.Espresso)

      val name = "Capuccino"
      val capuccino = Coffee.Other(name)
      val capuccinoObj = {
        val dbo = new BasicDBObject
        dbo.put("name", name)
        dbo.put("type", "Other")
        dbo
      }
      val serializedCapuccino = mongo.toMongoValue(capuccino)
      val deserializedCapuccino = mongo.fromMongoValue(capuccinoObj)
      serializedCapuccino must be(capuccinoObj)
      deserializedCapuccino must be(capuccino)
    }

    "serialize and deserialize enums" in {
      val mongo = TypedMongoFormat[Visitor]

      val anonObj = {
        val dbo = new BasicDBObject
        dbo.put("type", "Anonymous")
        dbo
      }
      val serializedAnon = mongo.toMongoValue(Visitor.Anonymous)
      val deserializedAnon = mongo.fromMongoValue(serializedAnon)
      serializedAnon must be(anonObj)
      deserializedAnon must be(Visitor.Anonymous)

      val email = "ian@sosafe.com"
      val password = "123456"
      val user = Visitor.User(email, password)
      val userObj = {
        val dbo = new BasicDBObject
        dbo.put("email", email)
        dbo.put("password", password)
        dbo.put("type", "User")
        dbo
      }
      val serializedUser = mongo.toMongoValue(user)
      val deserializedUser = mongo.fromMongoValue(userObj)
      serializedUser must be(userObj)
      deserializedUser must be(user)
    }

    "serialize and deserialize enumerations" in {
      // val mongo: TypedMongoFormat[Color.Value] = io.sphere.mongo.generic.deriveMongoFormat

      // // mongo java driver knows how to encode/decode Strings
      // val serializedObject = mongo.toMongoValue(Color.Red).asInstanceOf[String]
      // serializedObject must be("Red")

      // val enumValue = mongo.fromMongoValue(serializedObject)
      // enumValue must be(Color.Red)
    }
  }
