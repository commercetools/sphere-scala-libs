package io.sphere.json.generic

import cats.data.Validated.Valid
import cats.implicits._
import io.sphere.json.{JSON, JValidation, parseJSON}
import io.sphere.util.test._
import org.json4s._
import org.json4s.DefaultReaders.StringReader
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Shared test case logic for jsonTypeSwitch, usable from both Scala 2 and Scala 3. */
trait JsonTypeSwitchBehaviors { self: AnyWordSpec with Matchers =>
  import JsonTypeSwitchModels._

  def testDeriveASubsetOfASealedTrait(format: JSON[A]): Unit = {
    val b = B(123)
    val jsonB = format.write(b)
    val b2 = format.read(jsonB).expectValid
    b2 must be(b)

    val c = C(2345345)
    val jsonC = format.write(c)
    val c2 = format.read(jsonC).expectValid
    c2 must be(c)
  }

  def testDeriveSubsetWithMongoKey(format: JSON[A]): Unit = {
    val d = D(123)
    val json = format.write(d)
    val d2 = format.read(json)

    (json \ "type").as[String] must be("D2")
    d2 must be(Valid(d))
  }

  def testCombineSumTypes(format: JSON[Message]): Unit = {
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

  def testCustomSubtypeImpl(format: JSON[A]): Unit = {
    check[A](D(2345), """ {"type": "D2", "int": 2345 } """)(format)
    check[A](C(4), """ {"type": "C", "int": 4 } """)(format)
    check[A](B(34), """ {"type": "B", "field": "Custom-B-34" } """)(format)
  }

  def testPlatformFormattedNotificationCase(): Unit = {
    val formatSub2 = jsonTypeSwitch[SubTrait2, SubTrait2.O3.type, SubTrait2.O4.type](Nil)
    val formatSub3 = jsonTypeSwitch[SubTrait3, SubTrait3.O5.type, SubTrait3.O6.type](Nil)

    val typeSelectors = formatSub2.typeSelectors ++ formatSub3.typeSelectors
    val formatSuper: JSON[SuperTrait] = jsonTypeSwitch[SuperTrait, SubTrait1](typeSelectors)

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
