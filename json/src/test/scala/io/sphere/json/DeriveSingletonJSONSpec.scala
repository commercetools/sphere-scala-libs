package io.sphere.json

import cats.data.Validated.Valid
import io.sphere.json.generic._
import org.json4s.JValue
import org.json4s.JsonAST.JNothing
import org.json4s.jackson.JsonMethods.{compact, render}
import org.scalatest.{MustMatchers, WordSpec}

class DeriveSingletonJSONSpec extends WordSpec with MustMatchers {
  "DeriveSingletonJSON" must {
    "read normal singleton values" in {
      val user = getFromJSON[UserWithPicture](
        """
          {
            "userId": "foo-123",
            "pictureSize": "Medium",
            "pictureUrl": "http://exmple.com"
          }
        """)

      user must be (UserWithPicture("foo-123", Medium, "http://exmple.com"))
    }

    "fail to read if singleton value is unknown" in {
      a [JSONException] must be thrownBy getFromJSON[UserWithPicture](
        """
          {
            "userId": "foo-123",
            "pictureSize": "foo",
            "pictureUrl": "http://exmple.com"
          }
        """)
    }

    "write normal singleton values" in {
      val userJson = toJValue(UserWithPicture("foo-123", Medium, "http://exmple.com"))

      val Valid(expectedJson) = parseJSON("""
        {
          "userId": "foo-123",
          "pictureSize": "Medium",
          "pictureUrl": "http://exmple.com"
        }
        """)

      filter(userJson) must be (expectedJson)
    }

    "read custom singleton values" in {
      val user = getFromJSON[UserWithPicture](
        """
          {
            "userId": "foo-123",
            "pictureSize": "bar",
            "pictureUrl": "http://exmple.com"
          }
        """)

      user must be (UserWithPicture("foo-123", Custom, "http://exmple.com"))
    }

    "write custom singleton values" in {
      val userJson = toJValue(UserWithPicture("foo-123", Custom, "http://exmple.com"))

      val Valid(expectedJson) = parseJSON("""
        {
          "userId": "foo-123",
          "pictureSize": "bar",
          "pictureUrl": "http://exmple.com"
        }
        """)

      filter(userJson) must be (expectedJson)
    }

    "write and consequently read, which must produce the original value" in {
      val originalUser = UserWithPicture("foo-123", Medium, "http://exmple.com")
      val newUser = getFromJSON[UserWithPicture](compact(render(toJValue(originalUser))))

      newUser must be (originalUser)
    }

    "read and write sealed trait with only one subtype" in {
      val json = """
        {
          "userId": "foo-123",
          "pictureSize": "Medium",
          "pictureUrl": "http://example.com",
          "access": {
             "type": "Authorized",
             "project": "internal"
          }
        }
      """
      val user = getFromJSON[UserWithPicture](json)

      user must be (UserWithPicture("foo-123", Medium, "http://example.com", Some(Access.Authorized("internal"))))

      val newJson = toJValue[UserWithPicture](user)
      Valid(newJson) must be (parseJSON(json))

      val Valid(newUser) = fromJValue[UserWithPicture](newJson)
      newUser must be (user)
    }
  }

  private def filter(jvalue: JValue): JValue =
    jvalue.removeField {
      case (_, JNothing) => true
      case _ => false
    }
}

sealed abstract class PictureSize(val weight: Int, val height: Int)

case object Small extends PictureSize(100, 100)
case object Medium extends PictureSize(500, 450)
case object Big extends PictureSize(1024, 2048)

@JSONTypeHint("bar")
case object Custom extends PictureSize(1, 2)

object PictureSize {
  implicit val json: JSON[PictureSize] = deriveSingletonJSON[PictureSize]
}

sealed trait Access
object Access {
  // only one sub-type
  case class Authorized(project: String) extends Access

  implicit val json: JSON[Access] = deriveJSON
}


case class UserWithPicture(
  userId: String,
  pictureSize: PictureSize,
  pictureUrl: String,
  access: Option[Access] = None)

object UserWithPicture {
  implicit val json: JSON[UserWithPicture] = deriveJSON[UserWithPicture]
}
