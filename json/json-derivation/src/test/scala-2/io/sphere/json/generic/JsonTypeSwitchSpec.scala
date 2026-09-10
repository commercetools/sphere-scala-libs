package io.sphere.json.generic

import cats.data.Validated.Valid
import cats.syntax.traverse._
import io.sphere.json.{JSON, JValidation, parseJSON}
import io.sphere.util.test._
import org.json4s._
import org.json4s.DefaultReaders.StringReader
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonTypeSwitchSpec extends AnyWordSpec with Matchers {
  import JsonTypeSwitchModels._

  "jsonTypeSwitch" must {

    "derive a subset of a sealed trait" in {
      val format: JSON[A] = jsonTypeSwitch[A](List(sub[B], sub[C]))
      val b = B(123)
      val jsonB = format.write(b)
      val b2 = format.read(jsonB).expectValid
      b2 must be(b)

      val c = C(2345345)
      val jsonC = format.write(c)
      val c2 = format.read(jsonC).expectValid
      c2 must be(c)
    }

    "return an invalid result on malformed sum-type JSON" in {
      val format: JSON[A] = jsonTypeSwitch[A](List(sub[B], sub[C]))
      // missing discriminator field must accumulate an error, not throw
      format.read(JObject("int" -> JLong(1))).isInvalid must be(true)
      // non-string discriminator must accumulate an error, not throw
      format.read(JObject("type" -> JLong(5), "int" -> JLong(1))).isInvalid must be(true)
      // unknown discriminator value must accumulate an error, not throw
      format.read(JObject("type" -> JString("Nope"), "int" -> JLong(1))).isInvalid must be(true)
    }

    "derive a subset of a sealed trait with a JSONTypeHint" in {
      val format: JSON[A] = jsonTypeSwitch[A](List(sub[B], sub[D]))
      val d = D(123)
      val json = format.write(d)
      val d2 = format.read(json)

      (json \ "type").as[String] must be("D2")
      d2 must be(Valid(d))
    }

    "combine different sum types tree" in {
      val format: JSON[Message] = jsonTypeSwitch[Message](List(sub[TypeA], sub[TypeB]))
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

    "handle custom implementations for subtypes" in {
      implicit val jsonB: JSON[B] = customJsonB
      val format: JSON[A] = jsonTypeSwitch[A](List(sub[B], sub[D], sub[C]))
      check[A](D(2345), """ {"type": "D2", "int": 2345 } """)(format)
      check[A](C(4), """ {"type": "C", "int": 4 } """)(format)
      check[A](B(34), """ {"type": "B", "field": "Custom-B-34" } """)(format)
    }

    "handle the PlatformFormattedNotification case" in {
      val formatSub2 =
        jsonTypeSwitch[SubTrait2](List(sub[SubTrait2.O3.type], sub[SubTrait2.O4.type]))
      val formatSub3 =
        jsonTypeSwitch[SubTrait3](List(sub[SubTrait3.O5.type], sub[SubTrait3.O6.type]))

      val typeSelectors = formatSub2.typeSelectors ++ formatSub3.typeSelectors
      val formatSuper: JSON[SuperTrait] =
        jsonTypeSwitch[SuperTrait](sub[SubTrait1] :: typeSelectors)

      val objs =
        List[SuperTrait](
          SubTrait1.O1,
          SubTrait1.O2,
          SubTrait2.O3,
          SubTrait2.O4,
          SubTrait3.O5,
          SubTrait3.O6)

      val res = objs.map(formatSuper.write).map(formatSuper.read).sequence.expectValid

      res must be(objs)
    }
  }

  private def check[T](a: T, json: String)(format: JSON[T]): Unit = {
    val parsedJson = parseJSON(json).expectValid
    val json2 = format.write(a)
    json2 must be(parsedJson)
    format.read(json2).expectValid must be(a)
  }
}

object JsonTypeSwitchModels {
  sealed trait A
  case class B(int: Int) extends A
  object B { implicit val json: JSON[B] = deriveJSON }

  case class C(int: Int) extends A
  object C { implicit val json: JSON[C] = deriveJSON }

  @JSONTypeHint("D2") case class D(int: Int) extends A
  object D { implicit val json: JSON[D] = deriveJSON }

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
    implicit val json: JSON[SubTrait1] = deriveJSON
  }

  sealed trait SubTrait2 extends SuperTrait
  object SubTrait2 {
    case object O3 extends SubTrait2 { implicit val json: JSON[O3.type] = deriveJSON }
    case object O4 extends SubTrait2 { implicit val json: JSON[O4.type] = deriveJSON }
    implicit val json: JSON[SubTrait2] = deriveJSON
  }

  sealed trait SubTrait3 extends SuperTrait
  object SubTrait3 {
    case object O5 extends SubTrait3 { implicit val json: JSON[O5.type] = deriveJSON }
    case object O6 extends SubTrait3 { implicit val json: JSON[O6.type] = deriveJSON }
    implicit val json: JSON[SubTrait3] = deriveJSON
  }

  sealed trait SubTrait4 extends SuperTrait
  object SubTrait4 {
    case object O7 extends SubTrait4 { implicit val json: JSON[O7.type] = deriveJSON }
    case object O8 extends SubTrait4 { implicit val json: JSON[O8.type] = deriveJSON }
    implicit val json: JSON[SubTrait4] = deriveJSON
  }

  /** A custom JSON[B] for testing custom subtype implementations */
  val customJsonB: JSON[B] = new JSON[B] {
    override def read(jval: JValue): JValidation[B] = jval match {
      // JObject(List((field,JString(Custom-B-34)), (type,JString(B))))
      case JObject(fields) =>
        fields.collectFirst { case ("field", JString(s)) if s.startsWith("Custom-B-") => s } match {
          case Some(s) => Valid(B(s.stripPrefix("Custom-B-").toInt))
          case None => ???
        }
      case _ => ???
    }

    override def write(value: B): JValue =
      JObject(List("field" -> JString(s"Custom-B-${value.int}")))
  }
}
