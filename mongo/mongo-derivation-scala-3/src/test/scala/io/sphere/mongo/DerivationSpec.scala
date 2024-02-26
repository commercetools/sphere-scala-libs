package io.sphere.mongo

import io.sphere.mongo.generic.{FakeBson, FakeMongoFormat, SingleValue}
import io.sphere.mongo.generic.FakeMongoFormat.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class DerivationSpec extends AnyWordSpec with Matchers:

  "asdasd" in {

    case class Second(xx: Int)
    case class TopLvlClass(x: Int, str: String, asd: Second)

    val b = FakeMongoFormat.apply[TopLvlClass]

    val a = TopLvlClass(2234, "aasdasdsd", Second(234))
    val res = b.toFakeBson(a)

    val res2 = b.fromFakeBson(res)

    println(res2)
    println(res2 == a)
    println(res)

    sealed trait SealedTrait1
    case object Case1 extends SealedTrait1
    case object Case2 extends SealedTrait1
    case class Case3(x: Int) extends SealedTrait1

    val format2 = FakeMongoFormat[SealedTrait1]

    val bson1 = format2.toFakeBson(Case2)

    println(bson1)

    val sum1 = format2.fromFakeBson(bson1)

    println(sum1)
  }