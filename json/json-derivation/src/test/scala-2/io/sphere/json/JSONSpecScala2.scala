package io.sphere.json

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.syntax.apply._
import org.json4s.JsonAST._
import io.sphere.json.field
import io.sphere.json.generic._
import io.sphere.util.Money
import org.joda.time.DateTime
import org.scalatest.matchers.must.Matchers
import org.scalatest.funspec.AnyFunSpec

object JSONSpecScala2 {
  case object Singleton

  sealed abstract class SingletonEnum
  case object SingletonA extends SingletonEnum
  case object SingletonB extends SingletonEnum
  case object SingletonC extends SingletonEnum

  sealed trait Mixed
  case object SingletonMixed extends Mixed
  case class RecordMixed(i: Int) extends Mixed

  object ScalaEnum extends Enumeration {
    val One, Two, Three = Value
  }

  // case class Node(value: Option[List[Node]]) // JSON instances for recursive data types cannot be derived
}

class JSONSpecScala2 extends AnyFunSpec with Matchers {
  import JSONSpecScala2._

  it("must provide derived instances for singleton objects") {
    implicit val toSingletonJSON = toJsonSingleton(Singleton)
    implicit val fromSingletonJSON = fromJsonSingleton(Singleton)
    val json = s"""[${toJSON(Singleton)}]"""
    withClue(json) {
      fromJSON[Seq[Singleton.type]](json) must equal(Valid(Seq(Singleton)))
    }

    // ToJSON
    implicit val toSingleAJSON = toJsonSingleton(SingletonA)
    implicit val toSingleBJSON = toJsonSingleton(SingletonB)
    implicit val toSingleCJSON = toJsonSingleton(SingletonC)
    implicit val toSingleEnumJSON =
      toJsonSingletonEnumSwitch[SingletonEnum, SingletonA.type, SingletonB.type, SingletonC.type](
        Nil)
    // FromJSON
    implicit val fromSingleAJSON = fromJsonSingleton(SingletonA)
    implicit val fromSingleBJSON = fromJsonSingleton(SingletonB)
    implicit val fromSingleCJSON = fromJsonSingleton(SingletonC)
    implicit val fromSingleEnumJSON =
      fromJsonSingletonEnumSwitch[SingletonEnum, SingletonA.type, SingletonB.type, SingletonC.type](
        Nil)

    List(SingletonA, SingletonB, SingletonC).foreach { s: SingletonEnum =>
      fromJSON[SingletonEnum](toJSON(s)) must equal(Valid(s))
    }
  }
}
