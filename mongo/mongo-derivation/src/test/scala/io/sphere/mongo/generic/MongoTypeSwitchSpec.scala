package io.sphere.mongo.generic

import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.format.MongoFormat
import org.bson.BSONObject
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MongoTypeSwitchSpec extends AnyWordSpec with Matchers {
  import MongoTypeSwitchSpec._

  "mongoTypeSwitch" must {
    "derive a subset of a sealed trait" in {
      val format = mongoTypeSwitch[A, B, C](Nil)

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
      val format = mongoTypeSwitch[A, B, D](Nil)

      val d = D(123)
      val bson = format.toMongoValue(d).asInstanceOf[BSONObject]
      val d2 = format.fromMongoValue(bson)

      bson.get("type") must be("D2")
      d2 must be(d)

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
}
