package io.sphere.json

import java.util.UUID

import org.json4s._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ToJSONSpec extends AnyWordSpec with Matchers {

  case class User(id: UUID, firstName: String, age: Int)

  "ToJSON.apply" must {
    "create a ToJSON" in {
      implicit val encodeUser: ToJSON[User] = ToJSON.instance[User](u =>
        JObject(
          List(
            "id" -> toJValue(u.id),
            "first_name" -> toJValue(u.firstName),
            "age" -> toJValue(u.age)
          )))

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
}
