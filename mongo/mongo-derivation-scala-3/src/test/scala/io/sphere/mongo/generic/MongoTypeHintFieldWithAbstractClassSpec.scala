package io.sphere.mongo.generic

import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.format.{MongoFormat, fromMongo, toMongo}
import io.sphere.mongo.generic.DefaultMongoFormats.given
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MongoTypeHintFieldWithAbstractClassSpec extends AnyWordSpec with Matchers {
  import MongoTypeHintFieldWithAbstractClassSpec.*

  "MongoTypeHintField (with abstract class)" must {
    "allow to set another field to distinguish between types (toMongo)" in {
      val user = UserWithPicture("foo-123", Medium, "http://example.com")
      val expected = dbObj(
        "userId" -> "foo-123",
        "pictureSize" -> dbObj("pictureType" -> "Medium"),
        "pictureUrl" -> "http://example.com")

      val dbo = UserWithPicture.mongo.toMongoValue(user)
      dbo must be(expected)
    }

    "allow to set another field to distinguish between types (fromMongo)" in {
      val initialDbo = dbObj(
        "userId" -> "foo-123",
        "pictureSize" -> dbObj("pictureType" -> "Medium"),
        "pictureUrl" -> "http://example.com")

      val user = UserWithPicture.mongo.fromMongoValue(initialDbo)

      user must be(UserWithPicture("foo-123", Medium, "http://example.com"))

      val dbo = UserWithPicture.mongo.toMongoValue(user)
      dbo must be(initialDbo)
    }
  }
}

object MongoTypeHintFieldWithAbstractClassSpec {

  @MongoTypeHintField(value = "pictureType")
  sealed abstract class PictureSize
  case object Small extends PictureSize
  case object Medium extends PictureSize
  case object Big extends PictureSize

  case class UserWithPicture(userId: String, pictureSize: PictureSize, pictureUrl: String)

  object UserWithPicture {
    val mongo: TypedMongoFormat[UserWithPicture] = deriveMongoFormat[UserWithPicture]
  }
}
