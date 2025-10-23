package io.sphere.mongo

import com.mongodb.BasicDBObject
import io.sphere.mongo.format.{DefaultMongoFormats, MongoFormat}
import io.sphere.mongo.generic.{MongoAnnotationReader, MongoTypeHint}
import io.sphere.mongo.MongoUtils.dbObj
import DefaultMongoFormats.given
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.UUID

object ProductTypes {
  // For semi-automatic derivarion + default value argument
  case class Something(a: Option[Int], b: Int = 2)

  // For Automatic derivation with `derives`
  case class Frunfles(a: Option[Int], b: Int) derives MongoFormat

  // Union type field - doesn't compile!
  // case class Identifier(idOrKey: UUID | String) derives TypedMongoFormat
}

object SumTypes {
  object Color extends Enumeration {
    val Blue, Red, Yellow = Value
  }

  sealed trait Coffee derives MongoFormat

  object Coffee {
    case object Espresso extends Coffee

    case class Other(name: String) extends Coffee
  }

  enum Visitor derives MongoFormat {
    case User(email: String, password: String)
    case Anonymous
    @MongoTypeHint("Admin") case Administrator
  }
}

class SerializationTestForScala3Features extends AnyWordSpec with Matchers {
  "mongoProduct" must {
    import ProductTypes.*

    "generate a format that serializes optional fields with value None as BSON objects without that field (using derives)" in {
      // Using an automatically-derived type via new Scala 3 `derives` directive
      val frunfles = Frunfles(None, 1)

      val serializedObject =
        MongoFormat[Frunfles].toMongoValue(frunfles).asInstanceOf[BasicDBObject]

      serializedObject.keySet().contains("b") must be(true)
      serializedObject.keySet().contains("a") must be(false)
      MongoFormat[Frunfles].fromMongoValue(serializedObject) must be(frunfles)
    }

  }

  // Both sealed hierarchies and enums get this "type" field, even the Singleton cases.
  // Is this what we want, or should the Singleton cases be just a String?
  "mongoSum" must {
    import SumTypes.*

    "serialize and deserialize sealed hierarchies" in {
      val mongo = MongoFormat[Coffee]

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

    "mongoEnum" must {

      // There's an issue with the .getClass on enum objects, as they somehow refer to the main trait, instead of the object itself.
      // As this is a new feature compared to the scala 2 version I'd rather figure this issue out later.
      "serialize and deserialize scala3 enums" in pendingUntilFixed {
        val mongo = MongoFormat[Visitor]

        val serializedAnon = mongo.toMongoValue(Visitor.Anonymous)
        val deserializedAnon = mongo.fromMongoValue(serializedAnon)
        serializedAnon must be(dbObj("type" -> "Anonymous"))
        deserializedAnon must be(Visitor.Anonymous)

        val serializedAdmin = mongo.toMongoValue(Visitor.Administrator)
        val deserializedAdmin = mongo.fromMongoValue(serializedAdmin)
        serializedAdmin must be(dbObj("type" -> "Admin"))
        deserializedAdmin must be(Visitor.Administrator)

        val email = "ian@sosafe.com"
        val password = "123456"
        val user = Visitor.User(email, password)
        val serializedUser = mongo.toMongoValue(user)
        val deserializedUser = mongo.fromMongoValue(serializedUser)
        serializedUser must be(dbObj("email" -> email, "password" -> password, "type" -> "User"))
        deserializedUser must be(user)
      }

    }
  }
}
