import java.util.UUID

import io.sphere.json.ToJSONProduct._
import io.sphere.json._
import org.json4s._
import org.scalatest.{MustMatchers, WordSpec}


case class User(id: UUID, firstName: String, age: Int)


class ForProductNSpec extends WordSpec with MustMatchers {

  "forProductN helper methods" must {
    "serialize" in {
      implicit val encodeUser: ToJSON[User] = with3Fields(u ⇒ (
        "id" → u.id,
        "first_name" → u.firstName,
        "age" → u.age
      ))

      val id = UUID.randomUUID()
      val json = toJValue(User(id, "bidule", 109))
      json must be (JObject(List(
        "id" → JString(id.toString),
        "first_name" → JString("bidule"),
        "age" → JInt(109)
      )))
    }

    "serialize with apply" in {
      implicit val encodeUser: ToJSON[User] = ToJSON[User](u ⇒ JObject(
        "id" → toJValue(u.id) ::
        "first_name" → toJValue(u.firstName) ::
        "age" → toJValue(u.age) :: Nil
      ))

      val id = UUID.randomUUID()
      val json = toJValue(User(id, "bidule", 109))
      json must be (JObject(List(
        "id" → JString(id.toString),
        "first_name" → JString("bidule"),
        "age" → JInt(109)
      )))
    }

    "serialize with obj" in {
      implicit val encodeUser: ToJSON[User] = ToJSON.obj[User](u ⇒
        "id" → toJValue(u.id) ::
        "first_name" → toJValue(u.firstName) ::
        "age" → toJValue(u.age) :: Nil
      )

      val id = UUID.randomUUID()
      val json = toJValue(User(id, "bidule", 109))
      json must be (JObject(List(
        "id" → JString(id.toString),
        "first_name" → JString("bidule"),
        "age" → JInt(109)
      )))
    }
  }

}
