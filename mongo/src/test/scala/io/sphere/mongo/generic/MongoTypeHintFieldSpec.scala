package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.format.{MongoFormat, fromMongo, toMongo}
import org.scalatest.{MustMatchers, WordSpec}
import io.sphere.mongo.format.DefaultMongoFormats._

class MongoTypeHintFieldSpec extends WordSpec with MustMatchers {
  import MongoTypeHintFieldSpec._

  "MongoTypeHintField" must {
    "allow to set another field to distinguish between types" in {
      val initialDbo = dbObj(
        "userId" -> "foo-123",
        "pictureSize" -> dbObj(
          "pictureType" -> "Medium"),
        "pictureUrl" -> "http://example.com")

      val user = fromMongo[UserWithPicture](initialDbo)

      user must be (UserWithPicture("foo-123", Medium, "http://example.com"))

      val dbo = toMongo[UserWithPicture](user)
      dbo must be (initialDbo)
    }
  }
}

object MongoTypeHintFieldSpec {

  @MongoTypeHintField(value = "pictureType")
  sealed trait PictureSize
  @MongoTypeHintField(value = "pictureType")
  case object Small extends PictureSize
  @MongoTypeHintField(value = "pictureType")
  case object Medium extends PictureSize
  @MongoTypeHintField(value = "pictureType")
  case object Big extends PictureSize

  object PictureSize {
    implicit val mongo: MongoFormat[PictureSize] = deriveMongoFormat[PictureSize]
  }

  case class UserWithPicture(
    userId: String,
    pictureSize: PictureSize,
    pictureUrl: String)

  object UserWithPicture {
    implicit val mongo: MongoFormat[UserWithPicture] = mongoProduct
  }
}
