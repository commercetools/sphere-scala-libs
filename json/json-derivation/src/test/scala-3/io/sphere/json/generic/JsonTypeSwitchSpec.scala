package io.sphere.json.generic

import cats.data.Validated.Valid
import io.sphere.json.{JSON, JSONParseError, JValidation, deriveJSON}
import io.sphere.json.generic.jsonTypeSwitch
import org.json4s.JsonAST.JObject
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.json4s.*
import org.json4s.DefaultReaders.StringReader

class JsonTypeSwitchSpec extends AnyWordSpec with Matchers {
  import JsonTypeSwitchSpec.*

  "jsonTypeSwitch" must {

    {
      given JSON[B] = deriveJSON[B]
      "derive a subset of a sealed trait".withFormatters(
        newSyntax = jsonTypeSwitch[A, (B, C)],
        oldSyntax = jsonTypeSwitch[A, B, C](Nil)
      ) { format =>
        val b = B(123)
        val jsonB = format.write(b)

        val b2 = format.read(jsonB).getOrElse(null)

        b2 must be(b)

        val c = C(2345345)
        val jsonC = format.write(c)

        val c2 = format.read(jsonC).getOrElse(null)

        c2 must be(c)
      }
    }

    "derive a subset of a sealed trait with a mongoKey".withFormatters(
      newSyntax = jsonTypeSwitch[A, (B, D)],
      oldSyntax = jsonTypeSwitch[A, B, D](Nil)
    ) { format =>
      val d = D(123)
      val json = format.write(d)
      val d2 = format.read(json)

      (json \ "type").as[String] must be("D2")
      d2 must be(Valid(d))
    }

    "combine different sum types tree".withFormatters(
      newSyntax = jsonTypeSwitch[Message, (TypeA, TypeB)],
      oldSyntax = jsonTypeSwitch[Message, (TypeA, TypeB)]
    ) { format =>
      val m: Seq[Message] = List(
        TypeA.ClassA1(23),
        TypeA.ClassA2("world"),
        TypeB.ClassB1(valid = false),
        TypeB.ClassB2(Seq("a23", "c62")))

      val jsons = m.map(format.write)
      jsons must be(
        List(
          JObject("number" -> JLong(23), "type" -> JString("ClassA1")),
          JObject("name" -> JString("world"), "type" -> JString("ClassA2")),
          JObject("valid" -> JBool(false), "type" -> JString("ClassB1")),
          JObject(
            "references" -> JArray(List(JString("a23"), JString("c62"))),
            "type" -> JString("ClassB2"))
        ))

      val messages = jsons.map(format.read).map(_.toOption.get)
      messages must be(m)
    }

    {
      given JSON[B] = new JSON[B] {
        override def read(jval: JValue): JValidation[B] = jval match {
          case JObject(List(_, "field" -> JString(s"Custom-B-${n}"))) =>
            Valid(B(n.toInt))
          case _ => ???
        }

        override def write(value: B): JValue =
          JObject(List("field" -> JString(s"Custom-B-${value.int}")))
      }

      "handle custom implementations for subtypes".withFormatters(
        newSyntax = jsonTypeSwitch[A, (B, D, C)],
        oldSyntax = jsonTypeSwitch[A, B, D, C](Nil)
      ) { format =>
        List(D(2345), C(4), B(34)).foreach { value =>
          val json = format.write(value)
          format.read(json).getOrElse(null) must be(value)
        }
      }
    }
  }

  extension (string: String) {
    def withFormatters[A](newSyntax: JSON[A], oldSyntax: JSON[A])(f: JSON[A] => Any): Unit = {
      s"$string with newSyntax" in {
        f(newSyntax)
      }

      s"$string with oldSyntax" in {
        f(oldSyntax)
      }
    }
  }
}

object JsonTypeSwitchSpec {
  sealed trait A
  case class B(int: Int) extends A
  case class C(int: Int) extends A
  @JSONTypeHint("D2") case class D(int: Int) extends A

  trait Message

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
