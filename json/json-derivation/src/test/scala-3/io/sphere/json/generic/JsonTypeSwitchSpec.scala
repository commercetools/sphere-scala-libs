package io.sphere.json.generic

import cats.data.Validated.Valid
import cats.implicits.toTraverseOps
import io.sphere.json.{JSON, JSONParseError, JValidation, deriveJSON, parseJSON}
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
      ) {
        val b = B(123)
        val jsonB = JSON[A].write(b)

        val b2 = JSON[A].read(jsonB).getOrElse(null)

        b2 must be(b)

        val c = C(2345345)
        val jsonC = JSON[A].write(c)

        val c2 = JSON[A].read(jsonC).getOrElse(null)

        c2 must be(c)
      }
    }

    "derive a subset of a sealed trait with a mongoKey".withFormatters(
      newSyntax = jsonTypeSwitch[A, (B, D)],
      oldSyntax = jsonTypeSwitch[A, B, D](Nil)
    ) {
      val d = D(123)
      val json = JSON[A].write(d)
      val d2 = JSON[A].read(json)

      (json \ "type").as[String] must be("D2")
      d2 must be(Valid(d))
    }

    "combine different sum types tree".withFormatters(
      newSyntax = jsonTypeSwitch[Message, (TypeA, TypeB)],
      oldSyntax = jsonTypeSwitch[Message, TypeA, TypeB](Nil)
    ) {
      val m: Seq[Message] = List(
        TypeA.ClassA1(23),
        TypeA.ClassA2("world"),
        TypeB.ClassB1(valid = false),
        TypeB.ClassB2(Seq("a23", "c62")))

      val jsons = m.map(JSON[Message].write)
      jsons must be(
        List(
          JObject("number" -> JLong(23), "type" -> JString("ClassA1")),
          JObject("name" -> JString("world"), "type" -> JString("ClassA2")),
          JObject("valid" -> JBool(false), "type" -> JString("ClassB1")),
          JObject(
            "references" -> JArray(List(JString("a23"), JString("c62"))),
            "type" -> JString("ClassB2"))
        ))

      val messages = jsons.map(JSON[Message].read).map(_.toOption.get)
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
      ) {
        check[A](D(2345), """ {"type": "D2", "int": 2345 } """)
        check[A](C(4), """ {"type": "C", "int": 4 } """)
        check[A](B(34), """ {"type": "B", "field": "Custom-B-34" } """)
      }
    }

    "handle PlatformFormattedNotification case" in {

      type Trait234 = SubTrait2 *: (SubTrait3, SubTrait4)

      val formatSuper: JSON[SuperTrait] = jsonTypeSwitch[SuperTrait, SubTrait1 *: Trait234]

      val objs =
        List[SuperTrait](
          SubTrait1.O1,
          SubTrait1.O2,
          SubTrait2.O3,
          SubTrait2.O4,
          SubTrait3.O5,
          SubTrait3.O6,
          SubTrait4.O7)

      val res = objs.map(formatSuper.write).map(formatSuper.read).sequence.getOrElse(null)

      res must be(objs)

    }
  }

  def check[A](a: A, json: String)(using format: JSON[A]): Unit = {
    val parsedJson = parseJSON(json).getOrElse(null)
    val json2 = format.write(a)
    json2 must be(parsedJson)
    format.read(json2).getOrElse(null) must be(a)
  }

  type FormatTest[A] = JSON[A] ?=> Any
  extension (string: String) {
    def withFormatters[A](newSyntax: JSON[A], oldSyntax: JSON[A])(f: FormatTest[A]): Unit = {
      s"$string with newSyntax" in {
        f(using newSyntax)
      }

      s"$string with oldSyntax" in {
        f(using oldSyntax)
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

  trait SuperTrait

  sealed trait SubTrait1 extends SuperTrait

  object SubTrait1 {
    case object O1 extends SubTrait1

    case object O2 extends SubTrait1

    given JSON[SubTrait1] = deriveJSON
  }

  sealed trait SubTrait2 extends SuperTrait

  object SubTrait2 {
    case object O3 extends SubTrait2

    case object O4 extends SubTrait2

    given JSON[SubTrait2] = deriveJSON
  }

  sealed trait SubTrait3 extends SuperTrait

  object SubTrait3 {
    case object O5 extends SubTrait3

    case object O6 extends SubTrait3

    given JSON[SubTrait3] = deriveJSON
  }

  sealed trait SubTrait4 extends SuperTrait

  object SubTrait4 {
    case object O7 extends SubTrait4

    case object O8 extends SubTrait4

    given JSON[SubTrait4] = deriveJSON
  }
}
