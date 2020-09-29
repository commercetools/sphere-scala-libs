package io.sphere.json

import io.sphere.json.generic.{TypeSelectorContainer, deriveJSON, jsonTypeSwitch}
import org.json4s._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypesSwitchSpec extends AnyWordSpec with Matchers {
  import TypesSwitchSpec._

  "jsonTypeSwitch" must {
    "combine different sum types tree" in {
      val m: Seq[Message] = List(
        TypeA.ClassA1(23),
        TypeA.ClassA2("world"),
        TypeB.ClassB1(valid = false),
        TypeB.ClassB2(Seq("a23", "c62")))

      val jsons = m.map(Message.json.write)
      jsons must be (List(
        JObject("number" -> JLong(23), "type" -> JString("ClassA1")),
        JObject("name" -> JString("world"), "type" -> JString("ClassA2")),
        JObject("valid" -> JBool(false), "type" -> JString("ClassB1")),
        JObject("references" -> JArray(List(JString("a23"), JString("c62"))), "type" -> JString("ClassB2"))))

      val messages = jsons.map(Message.json.read).map(_.toOption.get)
      messages must be (m)
    }
  }

  "TypeSelectorContainer" must {
    "have information about type value discriminators" in {
      val selectors = Message.json.typeSelectors
      selectors.map(_.typeValue) must contain.allOf("ClassA1", "ClassA2", "TypeA", "ClassB1", "ClassB2", "TypeB")

      // I don't think it's useful to allow different type fields. How is it possible to deserialize one json
      // if different type fields are used?
      selectors.map(_.typeField) must be(List("type", "type", "type", "type", "type", "type"))

      selectors.map(_.clazz.getName) must contain.allOf(
        "io.sphere.json.TypesSwitchSpec$TypeA$ClassA1",
        "io.sphere.json.TypesSwitchSpec$TypeA$ClassA2",
        "io.sphere.json.TypesSwitchSpec$TypeA",
        "io.sphere.json.TypesSwitchSpec$TypeB$ClassB1",
        "io.sphere.json.TypesSwitchSpec$TypeB$ClassB2",
        "io.sphere.json.TypesSwitchSpec$TypeB"
      )
    }
  }

}

object TypesSwitchSpec {

  trait Message
  object Message {
    // this can be dangerous is the same class name is used in both sum types
    // ex if we define TypeA.Class1 && TypeB.Class1
    // as both will use the same type value discriminator
    implicit val json: JSON[Message] with TypeSelectorContainer =
      jsonTypeSwitch[Message, TypeA, TypeB](Nil)
  }

  sealed trait TypeA extends Message
  object TypeA {
    case class ClassA1(number: Int) extends TypeA
    case class ClassA2(name: String) extends TypeA
    implicit val json: JSON[TypeA] = deriveJSON[TypeA]
  }

  sealed trait TypeB extends Message
  object TypeB {
    case class ClassB1(valid: Boolean) extends TypeB
    case class ClassB2(references: Seq[String]) extends TypeB
    implicit val json: JSON[TypeB] = deriveJSON[TypeB]
  }
}
