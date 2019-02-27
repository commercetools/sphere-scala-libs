package io.sphere.mongo.generic

import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format._
import org.scalatest.{MustMatchers, WordSpec}
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.MongoUtils._

class DeriveMongoformatSpec extends WordSpec with MustMatchers {
  "deriving MongoFormat" must {
    "read normal singleton values" in {
      val user = fromMongo[UserWithPicture](
        dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj(
            "type" -> "Medium"),
          "pictureUrl" -> "http://example.com"))

      user must be (UserWithPicture("foo-123", Medium, "http://example.com"))
    }

    "read custom singleton values" in {
      val user = fromMongo[UserWithPicture](
        dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj(
            "type" -> "bar",
            "width" -> 23,
            "height" -> 30),
          "pictureUrl" -> "http://example.com"))

      user must be (UserWithPicture("foo-123", Custom(23, 30), "http://example.com"))
    }

    "fail to read if singleton value is unknown" in {
      a [Exception] must be thrownBy fromMongo[UserWithPicture](
        dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj(
            "type" -> "Unknown"),
          "pictureUrl" -> "http://example.com"))
    }

    "write normal singleton values" in {
      val dbo = toMongo[UserWithPicture](UserWithPicture("foo-123", Medium, "http://example.com"))
      dbo must be (dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj(
            "type" -> "Medium"),
          "pictureUrl" -> "http://example.com"))
    }

    "write custom singleton values" in {
      val dbo = toMongo[UserWithPicture](UserWithPicture("foo-123", Custom(23, 30), "http://example.com"))
      dbo must be (dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj(
            "type" -> "bar",
            "width" -> 23,
            "height" -> 30),
          "pictureUrl" -> "http://example.com"))
    }

    "write and consequently read, which must produce the original value" in {
      val originalUser = UserWithPicture("foo-123", Medium, "http://exmple.com")
      val newUser = fromMongo[UserWithPicture](toMongo[UserWithPicture](originalUser))

      newUser must be (originalUser)
    }
  }
}

sealed trait PictureSize
case object Small extends PictureSize
case object Medium extends PictureSize
case object Big extends PictureSize
@MongoTypeHint("bar")
case class Custom(width: Int, height: Int) extends PictureSize

object PictureSize {
  implicit val mongo: MongoFormat[PictureSize] = deriveMongoFormat[PictureSize]
}

case class UserWithPicture(userId: String, pictureSize: PictureSize, pictureUrl: String)

object UserWithPicture {
  implicit val mongo: MongoFormat[UserWithPicture] = mongoProduct(apply _)
}
