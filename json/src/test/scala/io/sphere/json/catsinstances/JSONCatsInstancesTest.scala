package io.sphere.json.catsinstances

import cats.syntax.invariant._
import cats.syntax.functor._
import cats.syntax.contravariant._
import io.sphere.json.JSON
import io.sphere.json._
import org.json4s.JsonAST
import org.json4s.JsonAST.JString
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JSONCatsInstancesTest extends AnyWordSpec with Matchers {
  import JSONCatsInstancesTest._

  "Invariant[JSON]" must {
    "allow imaping a default format" in {
      val myId = MyId("test")
      val json = toJValue(myId)
      json must be (JString("test"))
      val myNewId = getFromJValue[MyId](json)
      myNewId must be (myId)
    }
  }

  "Functor[FromJson] and Contramap[ToJson]" must {
    "allow mapping and contramapping a default format" in {
      val myId = MyId2("test")
      val json = toJValue(myId)
      json must be (JString("test"))
      val myNewId = getFromJValue[MyId2](json)
      myNewId must be (myId)
    }
  }
}

object JSONCatsInstancesTest {
  private val stringJson: JSON[String] = new JSON[String] {
    override def write(value: String): JsonAST.JValue = ToJSON[String].write(value)
    override def read(jval: JsonAST.JValue): JValidation[String] = FromJSON[String].read(jval)
  }

  case class MyId(id: String) extends AnyVal
  object MyId {
    implicit val json: JSON[MyId] = stringJson.imap(MyId.apply)(_.id)
  }

  case class MyId2(id: String) extends AnyVal
  object MyId2 {
    implicit val fromJson: FromJSON[MyId2] = FromJSON[String].map(apply)
    implicit val toJson: ToJSON[MyId2] = ToJSON[String].contramap(_.id)
  }
}
