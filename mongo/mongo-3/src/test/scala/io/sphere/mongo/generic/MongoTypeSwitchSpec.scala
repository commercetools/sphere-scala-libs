package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.*
import io.sphere.mongo.format.DefaultMongoFormats.given
import io.sphere.mongo.format.MongoFormat
import org.bson.BSONObject
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MongoTypeSwitchSpec extends AnyWordSpec with Matchers {

  sealed trait A
  case class B(int: Int) extends A
  case class C(int: Int) extends A
  @MongoTypeHint("D2") case class D(int: Int) extends A
  
  "mongoTypeSwitch" must {
    "derive a subset of a sealed trait" in {
      val format = generic.mongoTypeSwitch[A, (B, C)]()

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
      val format = generic.mongoTypeSwitch[A, (B, D)]()

      val d = D(123)
      val bson = format.toMongoValue(d).asInstanceOf[BSONObject]
      val d2 = format.fromMongoValue(bson)

      bson.get("type") must be("D2")
      d2 must be(d)

    }
  }
}
