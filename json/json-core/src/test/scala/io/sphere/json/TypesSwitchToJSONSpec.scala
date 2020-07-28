package io.sphere.json

import org.json4s._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypesSwitchToJSONSpec extends AnyWordSpec with Matchers {
  import TypesSwitchToJSONSpec._

  "SumTypesToJSON" should {
    "serialize sum types" in {
      val leafTypeA = LeafTypeA("hello")
      val jsonA = SumType.toJSon.write(leafTypeA)
      jsonA must be(JObject("type" -> JString("leafTypeA"), "b" -> JString("hello")))

      val leafTypeB = LeafTypeB
      val jsonB = SumType.toJSon.write(leafTypeB)
      jsonB must be(JObject("type" -> JString("leafTypeB")))

    }

    "not serialize types outside the sum type" in {
      "SumType.toJSon.write(TypeC)" mustNot compile
    }
  }
}

object TypesSwitchToJSONSpec {
  sealed trait SumTypeEx
  case class LeafTypeA(b: String) extends SumTypeEx
  object LeafTypeA {
    val toJSON: ToJSON[LeafTypeA] = l => JObject("b" -> JString(l.b))
  }

  case object LeafTypeB extends SumTypeEx {
    val toJSON: ToJSON[LeafTypeB.type] = _ => JObject()
  }

  case object TypeC

  object SumType {
    implicit val toJSon: ToJSON[SumTypeEx] =
      new TypesSwitchToJSON[SumTypeEx](
        subTypes = Map(
          TypeName(LeafTypeA.getClass) -> new ToJSONAux {
            override type B = LeafTypeA
            override val toJSON: ToJSON[B] = LeafTypeA.toJSON
            override val typeValue: String = "leafTypeA"
          },
          TypeName(LeafTypeB.getClass) -> new ToJSONAux {
            override type B = LeafTypeB.type
            override val toJSON: ToJSON[B] = LeafTypeB.toJSON
            override val typeValue: String = "leafTypeB"
          }
        ))
  }
}
