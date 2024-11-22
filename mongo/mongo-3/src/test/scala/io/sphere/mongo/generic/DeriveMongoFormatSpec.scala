package io.sphere.mongo.generic

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.sphere.mongo.generic.DefaultMongoFormats.given
import io.sphere.mongo.MongoUtils.*
import io.sphere.mongo.format.{MongoFormat, deriveMongoFormat}
import io.sphere.mongo.{fromMongo, toMongo}

class DeriveMongoFormatSpec extends AnyWordSpec with Matchers {
  import DeriveMongoFormatSpec.given
  import DeriveMongoFormatSpec.*

  "deriving MongoFormat" must {
    "read normal singleton values" in {
      val user = fromMongo[UserWithPicture](
        dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj("type" -> "Medium"),
          "pictureUrl" -> "http://example.com"))

      user must be(UserWithPicture("foo-123", Medium, "http://example.com"))
    }

    "read custom singleton values" in {
      val user = fromMongo[UserWithPicture](
        dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj("type" -> "bar", "width" -> 23, "height" -> 30),
          "pictureUrl" -> "http://example.com"))

      user must be(UserWithPicture("foo-123", Custom(23, 30), "http://example.com"))
    }

    "fail to read if singleton value is unknown" in {
      a[Exception] must be thrownBy fromMongo[UserWithPicture](
        dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj("type" -> "Unknown"),
          "pictureUrl" -> "http://example.com"))
    }

    "write normal singleton values" in {
      val dbo = toMongo[UserWithPicture](UserWithPicture("foo-123", Medium, "http://example.com"))
      dbo must be(
        dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj("type" -> "Medium"),
          "pictureUrl" -> "http://example.com"))
    }

    "write custom singleton values" in {
      val dbo =
        toMongo[UserWithPicture](UserWithPicture("foo-123", Custom(23, 30), "http://example.com"))
      dbo must be(
        dbObj(
          "userId" -> "foo-123",
          "pictureSize" -> dbObj("type" -> "bar", "width" -> 23, "height" -> 30),
          "pictureUrl" -> "http://example.com"))
    }

    "write and consequently read, which must produce the original value" in {
      val originalUser = UserWithPicture("foo-123", Medium, "http://exmple.com")
      val newUser = fromMongo[UserWithPicture](toMongo[UserWithPicture](originalUser))

      newUser must be(originalUser)
    }

    "read and write sealed trait with only one subtype" in {
      val dbo = dbObj(
        "userId" -> "foo-123",
        "pictureSize" -> dbObj("type" -> "Medium"),
        "pictureUrl" -> "http://example.com",
        "access" -> dbObj("type" -> "Authorized", "project" -> "internal")
      )
      val user = fromMongo[UserWithPicture](dbo)

      user must be(
        UserWithPicture(
          "foo-123",
          Medium,
          "http://example.com",
          Some(Access.Authorized("internal"))))
      val newDbo = toMongo[UserWithPicture](user)
      newDbo must be(dbo)

      val newUser = fromMongo[UserWithPicture](newDbo)
      newUser must be(user)
    }

    "fail to derive if trait is not sealed" in {
      // Sealed
      "implicit val mongo: MongoFormat[SealedSub] = deriveMongoFormat[SealedSub]" must compile
      // Not sealed
      "implicit val mongo: MongoFormat[NotSealed] = deriveMongoFormat[NotSealed]" mustNot compile
      // Sealed, but child is not sealed
      "implicit val mongo: MongoFormat[SealedParent] = deriveMongoFormat[SealedParent]" mustNot compile
    }
  }
}

object DeriveMongoFormatSpec {
  sealed trait PictureSize
  case object Small extends PictureSize
  case object Medium extends PictureSize
  case object Big extends PictureSize
  @MongoTypeHint(value = "bar")
  case class Custom(width: Int, height: Int) extends PictureSize

  given MongoFormat[PictureSize] = deriveMongoFormat[PictureSize]

  sealed trait Access
  object Access {
    // only one sub-type
    case class Authorized(project: String) extends Access

  }
  given MongoFormat[Access] = deriveMongoFormat

  case class UserWithPicture(
      userId: String,
      pictureSize: PictureSize,
      pictureUrl: String,
      access: Option[Access] = None)

  given MongoFormat[UserWithPicture] = deriveMongoFormat

  sealed trait SealedParent

  sealed trait SealedSub extends SealedParent
  case class Sub1(x: String) extends SealedSub
  case class Sub2(y: Int) extends SealedSub

  trait NotSealed extends SealedParent
  case class Sub3(x: String) extends NotSealed
  case class Sub4(y: Int) extends NotSealed
}
