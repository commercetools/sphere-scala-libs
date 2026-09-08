package io.sphere.mongo.generic

import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.MongoUtils.dbObj
import org.bson.BSONObject
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MongoTypeSwitchSpec extends AnyWordSpec with Matchers {
  import MongoTypeSwitchSpec._

  "mongoTypeSwitch" must {
    "derive a subset of a sealed trait" in {
      val format = mongoTypeSwitch[A](List(sub[B], sub[C]))

      val b = B(123)
      val bson = format.toMongoValue(b)

      val b2 = format.fromMongoValue(bson)

      b2 must be(b)

      val c = C(2345345)
      val bsonC = format.toMongoValue(c)

      val c2 = format.fromMongoValue(bsonC)

      c2 must be(c)
    }

    "derive a subset of a sealed trait with a mongoKey" in {
      val format = mongoTypeSwitch[A](List(sub[B], sub[D]))

      val d = D(123)
      val bson = format.toMongoValue(d).asInstanceOf[BSONObject]
      val d2 = format.fromMongoValue(bson)

      bson.get("type") must be("D2")
      d2 must be(d)

    }

    "honour a @MongoTypeHint on a subtype that is not a direct child" in {
      val format = mongoTypeSwitch[Top](List(sub[Leaf]))

      val bson = format.toMongoValue(Leaf(1)).asInstanceOf[BSONObject]
      bson.get("type") must be("LeafHint")
      format.fromMongoValue(bson) must be(Leaf(1))
    }

    "throw a descriptive error when the type field is missing" in {
      val format = mongoTypeSwitch[A](List(sub[B], sub[C]))
      val bson = dbObj("int" -> 1)
      val ex = intercept[Exception](format.fromMongoValue(bson))
      ex.getMessage must be("""Missing type field 'type' in DBObject '{"int": 1}'.""")
    }

    "throw a descriptive error for an unknown type field value" in {
      val format = mongoTypeSwitch[A](List(sub[B], sub[C]))
      val bson = dbObj("int" -> 1, "type" -> "Nope")
      val ex = intercept[Exception](format.fromMongoValue(bson))
      ex.getMessage must be(
        """Invalid type value 'Nope' in DBObject '{"int": 1, "type": "Nope"}'.""")
    }
  }
}

object MongoTypeSwitchSpec {
  sealed trait A
  case class B(int: Int) extends A
  object B {
    implicit val mongo: MongoFormat[B] = deriveMongoFormat
  }
  case class C(int: Int) extends A
  object C {
    implicit val mongo: MongoFormat[C] = deriveMongoFormat
  }
  @MongoTypeHint("D2") case class D(int: Int) extends A
  object D {
    implicit val mongo: MongoFormat[D] = deriveMongoFormat
  }

  sealed trait Top
  sealed trait Mid extends Top
  @MongoTypeHint("LeafHint") case class Leaf(int: Int) extends Mid
  object Leaf {
    implicit val mongo: MongoFormat[Leaf] = deriveMongoFormat
  }
}
