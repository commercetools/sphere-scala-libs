package io.sphere.json

import java.util.UUID

import org.json4s._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ToJSONSpec extends AnyWordSpec with Matchers {

  case class User(id: UUID, firstName: String, age: Int)
  object User {
    implicit val encoder: ToJSON[User] = ToJSON.instance[User](u =>
      JObject(
        List(
          "id" -> toJValue(u.id),
          "first_name" -> toJValue(u.firstName),
          "age" -> toJValue(u.age)
        )))
  }

  "ToJSON.apply" must {
    "create a ToJSON" in {
      val id = UUID.randomUUID()
      val json = toJValue(User(id, "bidule", 109))
      json must be(
        JObject(
          List(
            "id" -> JString(id.toString),
            "first_name" -> JString("bidule"),
            "age" -> JLong(109)
          )))
    }
  }

  "ToJSON.tapped" must {
    "call the passed side effect every n-th time" in {
      val uuids = Array.fill(3)(UUID.randomUUID())
      val firstName = "Ann"

      var tapCount = 0
      def tappedEncoder: ToJSON[User] = ToJSON.tapped[User](2) { json =>
        json mustBe
          JObject(
            List(
              "id" -> JString(uuids(1).toString),
              "first_name" -> JString(firstName),
              "age" -> JLong(1)
            ))
        tapCount = tapCount + 1
      }(User.encoder)
      implicit lazy val encoder: ToJSON[User] = tappedEncoder

      (0 to 2).foreach(i => toJValue(User(uuids(i), firstName, i)))
      tapCount mustBe 1
    }
  }
}
