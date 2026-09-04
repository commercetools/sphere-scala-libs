package io.sphere.json

import cats.data.Validated.Valid
import io.sphere.json.generic._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

object JSONSingletonSpec {
  case object Singleton

  sealed abstract class SingletonEnum
  case object SingletonA extends SingletonEnum
  case object SingletonB extends SingletonEnum
  case object SingletonC extends SingletonEnum
}

class JSONSingletonSpec extends AnyFunSpec with Matchers {
  import JSONSingletonSpec._

  it("must provide derived instances for singleton objects") {
    implicit val toSingletonJSON: ToJSON[Singleton.type] = toJsonSingleton(Singleton)
    implicit val fromSingletonJSON: FromJSON[Singleton.type] = fromJsonSingleton(Singleton)
    val json = s"""[${toJSON(Singleton)}]"""
    withClue(json) {
      fromJSON[Seq[Singleton.type]](json) must equal(Valid(Seq(Singleton)))
    }

    // ToJSON
    implicit val toSingleAJSON: ToJSON[SingletonA.type] = toJsonSingleton(SingletonA)
    implicit val toSingleBJSON: ToJSON[SingletonB.type] = toJsonSingleton(SingletonB)
    implicit val toSingleCJSON: ToJSON[SingletonC.type] = toJsonSingleton(SingletonC)
    implicit val toSingleEnumJSON: ToJSON[SingletonEnum] =
      toJsonSingletonEnumSwitch[SingletonEnum, SingletonA.type, SingletonB.type, SingletonC.type](
        Nil)
    // FromJSON
    implicit val fromSingleAJSON: FromJSON[SingletonA.type] = fromJsonSingleton(SingletonA)
    implicit val fromSingleBJSON: FromJSON[SingletonB.type] = fromJsonSingleton(SingletonB)
    implicit val fromSingleCJSON: FromJSON[SingletonC.type] = fromJsonSingleton(SingletonC)
    implicit val fromSingleEnumJSON: FromJSON[SingletonEnum] =
      fromJsonSingletonEnumSwitch[SingletonEnum, SingletonA.type, SingletonB.type, SingletonC.type](
        Nil)

    List(SingletonA, SingletonB, SingletonC).foreach { s: SingletonEnum =>
      fromJSON[SingletonEnum](toJSON(s)) must equal(Valid(s))
    }
  }
}
