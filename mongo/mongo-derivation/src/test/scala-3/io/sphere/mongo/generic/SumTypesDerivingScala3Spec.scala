package io.sphere.mongo.generic

import com.mongodb.DBObject
import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.generic.deriveMongoFormat
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format.DefaultMongoFormats.given
import org.bson.BSONObject
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SumTypesDerivingScala3Spec extends AnyWordSpec with Matchers {
  import SumTypesDerivingScala3Spec.*

  "Serializing sum types" must {

    "use custom field & values" in {
      check(Color4.format, Color4.Red, dbObj("color" -> "red"))

      check(Color4.format, Color4.Custom("2356"), dbObj("color" -> "custom", "rgb" -> "2356"))
    }

    "nested trait case 1: @MongoTypeHintField only on a subtype case class" in {
      // This was compile time checked in scala 2, so far I couldn't make it work easily, but it will fail during test running
      // TODO make this fail in compile time or figure out a way to resolve nested @MongoTypeHintFields
      val exception = intercept[Exception](deriveMongoFormat[Color5])

      exception.getMessage must be(
        "SubType: Custom has a different @MongoTypeHintField then its SuperType")
    }

    "nested trait case 2: @MongoTypeHintField on a subtype case class and the supertype" in {
      // This was compile time checked in scala 2, so far I couldn't make it work easily, but it will fail during test running
      // TODO make this fail in compile time or figure out a way to resolve nested @MongoTypeHintFields
      val exception = intercept[Exception](deriveMongoFormat[Color6])

      exception.getMessage must be(
        "SubType: MyColor has a different @MongoTypeHintField then its SuperType")
    }

    "do not use sealed trait info when using a case class directly" in {
      check(Color8.format, Color8.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))

      check(Color8.Custom.format, Color8.Custom("2356"), dbObj("rgb" -> "2356"))

      // unless annotated

      check(
        Color8.format,
        Color8.CustomAnnotated("1111"),
        dbObj("type" -> "CustomAnnotated", "rgb" -> "1111"))

      // I decided to drop adding the type field to case classes directly.
      // It's not used and the trait should handle it
      check(Color8.CustomAnnotated.format, Color8.CustomAnnotated("2356"), dbObj("rgb" -> "2356"))
    }

    "allow for providing custom instances for objects" in {
      check(Color10.format, Color10.Red, dbObj("type" -> "Red", "extraField" -> "panda"))

      check(Color10.format, Color10.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))
    }

    "allow for providing custom instances for classes" in {
      check(Color11.format, Color11.Red, dbObj("type" -> "Red"))

      check(
        Color11.format,
        Color11.Custom("2356"),
        dbObj("type" -> "Custom", "rgb" -> "2356", "extraField" -> "panda"))
    }

    "allow for providing custom instances for classes when the class has a type parameter with an upper bound" in {
      check(ColorUpperBound.format, ColorUpperBound.Red, dbObj("type" -> "Red"))

      check(
        ColorUpperBound.format,
        ColorUpperBound.Custom("2356"),
        dbObj("type" -> "Custom", "rgb" -> "2356", "extraField" -> "panda"))
    }

    "allow for providing custom instances for classes when the class has an unbounded type parameter" in {
      check(ColorUnbound.format, ColorUnbound.Red, dbObj("type" -> "Red"))

      check(
        ColorUnbound.format,
        ColorUnbound.Custom("2356"),
        dbObj("type" -> "Custom", "rgb" -> "2356", "extraField" -> "panda"))
    }

  }
}

object SumTypesDerivingScala3Spec {
  import Matchers.*

  def check[A, B <: A](format: MongoFormat[A], b: B, dbo: DBObject): Assertion = {
    val serialized = format.toMongoValue(b)
    serialized must be(dbo)

    format.fromMongoValue(serialized) must be(b)
  }

  @MongoTypeHintField("color")
  sealed trait Color4
  object Color4 {
    @MongoTypeHint("red") case object Red extends Color4
    @MongoTypeHint("custom") case class Custom(rgb: String) extends Color4
    val format = deriveMongoFormat[Color4]
  }

  @MongoTypeHintField("color")
  sealed trait Color5
  object Color5 {
    @MongoTypeHint("red")
    case object Red extends Color5
    @MongoTypeHintField("color-custom")
    @MongoTypeHint("custom")
    case class Custom(rgb: String) extends Color5
  }

  @MongoTypeHintField("color")
  sealed trait Color6
  object Color6 {
    @MongoTypeHintField("custom-color")
    sealed abstract class MyColor extends Color6
    @MongoTypeHint("mapped-red")
    case object Red extends MyColor
    @MongoTypeHint("custom")
    case class Custom(rgb: String) extends MyColor
  }

  sealed trait Color8
  object Color8 {
    // the formats must use `lazy` to make this code compile

    case object Red extends Color8
    case class Custom(rgb: String) extends Color8
    object Custom {
      lazy val format = deriveMongoFormat[Custom]
    }
    @MongoTypeHintField("type")
    case class CustomAnnotated(rgb: String) extends Color8
    object CustomAnnotated {
      lazy val format = deriveMongoFormat[CustomAnnotated]
    }
    lazy val format = deriveMongoFormat[Color8]
  }

  sealed trait Color10
  object Color10 {
    case object Red extends Color10
    case class Custom(rgb: String) extends Color10

    implicit val redFormatter: MongoFormat[Red.type] = new MongoFormat[Red.type] {
      override def toMongoValue(a: Red.type): Any =
        dbObj("type" -> "Red", "extraField" -> "panda")
      override def fromMongoValue(any: Any): Red.type = Red
    }
    val format = deriveMongoFormat[Color10]
  }

  sealed trait Color11
  object Color11 {
    case object Red extends Color11
    case class Custom(rgb: String) extends Color11

    implicit val customFormatter: MongoFormat[Custom] = new MongoFormat[Custom] {
      override def toMongoValue(a: Custom): Any =
        dbObj("type" -> "Custom", "rgb" -> a.rgb, "extraField" -> "panda")
      override def fromMongoValue(any: Any): Custom =
        Custom(any.asInstanceOf[BSONObject].get("rgb").asInstanceOf[String])
    }
    val format = deriveMongoFormat[Color11]
  }

  sealed trait ColorUpperBound
  object ColorUpperBound {

    sealed trait Bound
    case object B1 extends Bound
    case class B2(int: Int) extends Bound

    case object Red extends ColorUpperBound
    case class Custom[Type1 <: Bound](rgb: String) extends ColorUpperBound

    implicit def customFormatter[A <: Bound]: MongoFormat[Custom[A]] =
      new MongoFormat[Custom[A]] {
        override def toMongoValue(a: Custom[A]): Any =
          dbObj("type" -> "Custom", "rgb" -> a.rgb, "extraField" -> "panda")
        override def fromMongoValue(any: Any): Custom[A] =
          Custom(any.asInstanceOf[BSONObject].get("rgb").asInstanceOf[String])
      }

    val format = deriveMongoFormat[ColorUpperBound]
  }

  sealed trait ColorUnbound
  object ColorUnbound {
    case object Red extends ColorUnbound
    case class Custom[A](rgb: String) extends ColorUnbound

    implicit def customFormatter[A]: MongoFormat[Custom[A]] = new MongoFormat[Custom[A]] {
      override def toMongoValue(a: Custom[A]): Any =
        dbObj("type" -> "Custom", "rgb" -> a.rgb, "extraField" -> "panda")
      override def fromMongoValue(any: Any): Custom[A] =
        Custom(any.asInstanceOf[BSONObject].get("rgb").asInstanceOf[String])
    }

    val format = deriveMongoFormat[ColorUnbound]
  }
}
