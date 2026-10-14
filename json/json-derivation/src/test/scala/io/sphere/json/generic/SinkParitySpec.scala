package io.sphere.json
package generic

import org.json4s.JsonAST._
import org.json4s.jackson.compactJson
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** The derived writers have two implementations of the same thing: `write`, which builds a
  * `JValue`, and `writeTo`, which writes characters straight into a `JsonSink`. Everything else in
  * the suite exercises `write` and compares `JValue`s -- and `JObject.equals` compares `obj.toSet`,
  * so it would not even notice a field-order change.
  *
  * This spec compares the two paths **byte for byte** on the annotation and sum-type features whose
  * sink implementations are non-trivial.
  */
object SinkParitySpec {

  case class Inner(a: String, b: Int)
  object Inner {
    implicit val json: JSON[Inner] = deriveJSON
  }

  case class Embeds(name: String, @JSONEmbedded inner: Inner)
  object Embeds {
    implicit val json: JSON[Embeds] = deriveJSON
  }

  case class EmbedsOpt(name: String, @JSONEmbedded inner: Option[Inner] = None)
  object EmbedsOpt {
    implicit val json: JSON[EmbedsOpt] = deriveJSON
  }

  case class OnlyEmbedded(@JSONEmbedded inner: Inner)
  object OnlyEmbedded {
    implicit val json: JSON[OnlyEmbedded] = deriveJSON
  }

  case class Annotated(
      @JSONKey("renamed") original: String,
      @JSONIgnore skipped: String = "default",
      maybe: Option[String],
      quoted: String)
  object Annotated {
    implicit val json: JSON[Annotated] = deriveJSON
  }

  @JSONTypeHintField("kind")
  sealed trait Shape
  case class Circle(r: Double) extends Shape
  @JSONTypeHint("SQ")
  case class Square(side: Int) extends Shape
  case object Dot extends Shape
  object Shape {
    implicit val json: JSON[Shape] = deriveJSON
  }

  // A sum type on the *default* hint field, where the case object's own `"type"` and the
  // switch's `"type"` are the same field -- the switch must not write a second one.
  sealed trait Plain
  case class PlainA(v: Int) extends Plain
  case object PlainB extends Plain
  object Plain {
    implicit val json: JSON[Plain] = deriveJSON
  }

  // A subtype whose hint field differs from the switch's, so the switch has to add its own.
  sealed trait Outer
  @JSONTypeHintField("innerKind")
  case class OwnHint(x: Int) extends Outer
  case class NoHint(y: Int) extends Outer
  object Outer {
    implicit val json: JSON[Outer] = deriveJSON
  }

  // Hand-written subtype instance that does NOT write the type field; the switch supplies it.
  sealed trait Custom
  case class CustomA(n: Int) extends Custom
  case class CustomB(n: Int) extends Custom
  object Custom {
    implicit val customA: JSON[CustomA] = deriveJSON
    implicit val customB: JSON[CustomB] = new JSON[CustomB] {
      def read(jval: JValue): JValidation[CustomB] =
        fromJValue[Int](jval \ "n").map(CustomB.apply)
      def write(value: CustomB): JValue = JObject(List("custom" -> JString(s"B-${value.n}")))
    }
    implicit val json: JSON[Custom] = jsonTypeSwitch[Custom](List(sub[CustomA], sub[CustomB]))
  }

  case class Wrapper(shape: Shape, shapes: List[Shape], byName: Map[String, Shape])
  object Wrapper {
    implicit val json: JSON[Wrapper] = deriveJSON
  }
}

class SinkParitySpec extends AnyWordSpec with Matchers {
  import SinkParitySpec._

  /** `toJSON` goes through `writeTo`; `compactJson(toJValue(..))` is the pre-sink path. */
  private def parity[A: JSON](a: A): Unit = {
    val viaSink = toJSON(a)
    val viaTree = compactJson(toJValue(a))
    withClue(s"for $a: ")(viaSink mustEqual viaTree)
  }

  "the sink path" must {
    "match the JValue path for plain products" in {
      parity(Inner("x", 1))
      parity(Inner("", 0))
      parity(Inner("needs \"escaping\"\n", -1))
    }

    "match the JValue path for @JSONEmbedded" in {
      parity(Embeds("n", Inner("a", 2)))
      parity(OnlyEmbedded(Inner("a", 2)))
    }

    "match the JValue path for an absent @JSONEmbedded, which must not leave a dangling comma" in {
      parity(EmbedsOpt("n", None))
      parity(EmbedsOpt("n", Some(Inner("a", 2))))
    }

    "match the JValue path for @JSONKey, @JSONIgnore and absent options" in {
      parity(Annotated("v", "ignored", None, "q"))
      parity(Annotated("v", "ignored", Some("here"), "q"))
    }

    "match the JValue path for a type switch over case classes and case objects" in {
      parity[Shape](Circle(1.5))
      parity[Shape](Square(3))
      parity[Shape](Dot)
    }

    "match the JValue path for a case object on the default hint field" in {
      parity[Plain](PlainA(1))
      parity[Plain](PlainB)
    }

    "match the JValue path when the subtype's own hint field differs from the switch's" in {
      parity[Outer](OwnHint(1))
      parity[Outer](NoHint(2))
    }

    "match the JValue path for a hand-written subtype instance" in {
      parity[Custom](CustomA(1))
      parity[Custom](CustomB(2))
    }

    "match the JValue path for switches nested in collections and maps" in {
      parity(Wrapper(Dot, List(Circle(1.0), Square(2), Dot), Map("a" -> Dot, "b" -> Circle(0.5))))
      parity(Wrapper(Circle(1.0), Nil, Map.empty))
    }
  }

  "a type switch" must {
    "write the hint exactly once for a case object that writes it itself" in {
      val json = toJSON[Plain](PlainB)
      json.sliding(6).count(_ == "\"type\"") mustEqual 1
      json mustEqual """{"type":"PlainB"}"""
    }

    "still round-trip through the sink output" in {
      List[Shape](Circle(1.5), Square(3), Dot).foreach { s =>
        getFromJSON[Shape](toJSON[Shape](s)) mustEqual s
      }
      getFromJSON[Outer](toJSON[Outer](OwnHint(1))) mustEqual OwnHint(1)
      getFromJSON[Outer](toJSON[Outer](NoHint(2))) mustEqual NoHint(2)
      getFromJSON[Plain](toJSON[Plain](PlainB)) mustEqual PlainB
    }
  }
}
