package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.format.{MongoFormat, fromMongo, toMongo}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.sphere.mongo.format.DefaultMongoFormats._

class MongoTypeHintFieldWithSealedTraitSpec extends AnyWordSpec with Matchers {
  import MongoTypeHintFieldWithSealedTraitSpec._

  "MongoTypeHintField (with sealed trait)" must {
    "allow to set another field to distinguish between types (toMongo)" in {
      val user = UserWithPicture("foo-123", Medium, "http://example.com")
      val expected = dbObj(
        "userId" -> "foo-123",
        "pictureSize" -> dbObj("pictureType" -> "Medium"),
        "pictureUrl" -> "http://example.com")

      val dbo = toMongo[UserWithPicture](user)
      dbo must be(expected)
    }

    "allow to set another field to distinguish between types (fromMongo)" in {
      val initialDbo = dbObj(
        "userId" -> "foo-123",
        "pictureSize" -> dbObj("pictureType" -> "Medium"),
        "pictureUrl" -> "http://example.com")

      val user = fromMongo[UserWithPicture](initialDbo)

      user must be(UserWithPicture("foo-123", Medium, "http://example.com"))

      val dbo = toMongo[UserWithPicture](user)
      dbo must be(initialDbo)
    }
  }
}

object MongoTypeHintFieldWithSealedTraitSpec {

  // issue https://github.com/commercetools/sphere-scala-libs/issues/10
  // @MongoTypeHintField must be repeated for all sub-classes
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

  case class UserWithPicture(userId: String, pictureSize: PictureSize, pictureUrl: String)

  object UserWithPicture {
    implicit val mongo: MongoFormat[UserWithPicture] = mongoProduct(apply _)
  }
}
