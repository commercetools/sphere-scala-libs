package io.sphere.json

import io.sphere.json._
import org.json4s.{JObject, JValue}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SphereJsonExample extends AnyWordSpec with Matchers {

  case class User(name: String, age: Int, location: String)

  object User {

    // update https://github.com/commercetools/sphere-scala-libs/blob/master/json/README.md in case of changed
    implicit val json: JSON[User] = new JSON[User] {
      import cats.data.ValidatedNel
      import cats.syntax.apply._

      def read(jval: JValue): ValidatedNel[JSONError, User] = jval match {
        case o: JObject =>
          (field[String]("name")(o),
           field[Int]("age")(o),
           field[String]("location")(o)).mapN(User.apply)
        case _ => fail("JSON object expected.")
      }

      def write(u: User): JValue = JObject(List(
        "name" -> toJValue(u.name),
        "age" -> toJValue(u.age),
        "location" -> toJValue(u.location)
      ))
    }
  }

  "JSON[User]" should {
    "serialize and deserialize an user" in {
      val user = User("name", 23, "earth")
      val json = toJSON(user)
      parseJSON(json).isValid should be (true)

      val newUser = getFromJSON[User](json)
      newUser should be (user)
    }
  }

}
