package io.sphere.mongo.catsinstances

import cats.syntax.invariant.*
import io.sphere.mongo.{fromMongo, toMongo}
import io.sphere.mongo.format.*
import io.sphere.mongo.format.DefaultMongoFormats.given
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MongoFormatCatsInstancesTest extends AnyWordSpec with Matchers {
  import MongoFormatCatsInstancesTest.*

  "Invariant[MongoFormat]" must {
    "allow imaping a default format" in {
      val myId = MyId("test")
      val dbo = toMongo(myId)
      dbo.asInstanceOf[String] must be("test")
      val myNewId = fromMongo[MyId](dbo)
      myNewId must be(myId)
    }
  }
}

object MongoFormatCatsInstancesTest {
  case class MyId(id: String) extends AnyVal
  object MyId {
    implicit val mongo: MongoFormat[MyId] = MongoFormat[String].imap(MyId.apply)(_.id)
  }
}
