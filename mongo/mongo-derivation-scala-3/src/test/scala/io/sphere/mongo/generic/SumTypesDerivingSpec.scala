package io.sphere.mongo.generic

import com.mongodb.DBObject
import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.generic.DefaultMongoFormats.given
import org.bson.BSONObject
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SumTypesDerivingSpec extends AnyWordSpec with Matchers {
  import SumTypesDerivingSpec.*

  "Serializing sum types" must {
    "use 'type' as default field" in {
      check(Color1.format, Color1.Red, dbObj("type" -> "Red"))

      check(Color1.format, Color1.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))
    }

    "use custom field" in {
      check(Color2.format, Color2.Red, dbObj("color" -> "Red"))

      check(Color2.format, Color2.Custom("2356"), dbObj("color" -> "Custom", "rgb" -> "2356"))
    }

    "use custom values" in {
      check(Color3.format, Color3.Red, dbObj("type" -> "red"))

      check(Color3.format, Color3.Custom("2356"), dbObj("type" -> "custom", "rgb" -> "2356"))
    }

    "use custom field & values" in {
      check(Color4.format, Color4.Red, dbObj("color" -> "red"))

      check(Color4.format, Color4.Custom("2356"), dbObj("color" -> "custom", "rgb" -> "2356"))
    }

    "not allow specifying different custom field" in pendingUntilFixed {
      // to serialize Custom, should we use type "color" or "color-custom"?
      "deriveMongoFormat[Color5]" mustNot compile
    }

    "not allow specifying different custom field on intermediate level" in {
      // to serialize Custom, should we use type "color" or "color-custom"?
      "deriveMongoFormat[Color6]" mustNot compile
    }

    "use intermediate level" in {
      Color7.format
    }

    "do not use sealed trait info when using a case class directly" in {
      check(Color8.format, Color8.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))

      check(Color8.Custom.format, Color8.Custom("2356"), dbObj("rgb" -> "2356"))

      // unless annotated

      check(
        Color8.format,
        Color8.CustomAnnotated("2356"),
        dbObj("type" -> "CustomAnnotated", "rgb" -> "2356"))
// TODO should this be a thing?
//      check(
//        Color8.CustomAnnotated.format,
//        Color8.CustomAnnotated("2356"),
//        dbObj("type" -> "CustomAnnotated", "rgb" -> "2356"))
    }

    "use default values if custom values are empty" in {
      check(Color9.format, Color9.Red, dbObj("type" -> "Red"))

      check(Color9.format, Color9.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))
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

object SumTypesDerivingSpec {
  import Matchers.*

  def check[A, B <: A](format: TypedMongoFormat[A], b: B, dbo: DBObject): Assertion = {
    val serialized = format.toMongoValue(b)
    serialized must be(dbo)

    format.fromMongoValue(serialized) must be(b)
  }

  sealed trait Color1
  object Color1 {
    case object Red extends Color1
    case class Custom(rgb: String) extends Color1
    val format = deriveMongoFormat[Color1]
  }

  @MongoTypeHintField("color")
  sealed trait Color2
  object Color2 {
    case object Red extends Color2
    case class Custom(rgb: String) extends Color2
    val format = deriveMongoFormat[Color2]
  }

  sealed trait Color3
  object Color3 {
    @MongoTypeHint("red") case object Red extends Color3
    @MongoTypeHint("custom") case class Custom(rgb: String) extends Color3
    val format = deriveMongoFormat[Color3]
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
    @MongoTypeHintField("color-custom")
    abstract class MyColor extends Color6
    @MongoTypeHint("red")
    case object Red extends MyColor
    @MongoTypeHint("custom")
    case class Custom(rgb: String) extends MyColor
  }

  sealed trait Color7
  sealed trait Color7a extends Color7
  object Color7 {
    case object Red extends Color7a
    case class Custom(rgb: String) extends Color7a
    def format = deriveMongoFormat[Color7]
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

  sealed trait Color9
  object Color9 {
    @MongoTypeHint("")
    case object Red extends Color9
    @MongoTypeHint("  ")
    case class Custom(rgb: String) extends Color9
    val format = deriveMongoFormat[Color9]
  }

  sealed trait Color10
  object Color10 {
    case object Red extends Color10
    case class Custom(rgb: String) extends Color10

    implicit val redFormatter: TypedMongoFormat[Red.type] = new TypedMongoFormat[Red.type] {
      override def toMongoValue(a: Red.type): MongoType =
        dbObj("type" -> "Red", "extraField" -> "panda")
      override def fromMongoValue(any: MongoType): Red.type = Red
    }
    val format = deriveMongoFormat[Color10]
  }

  sealed trait Color11
  object Color11 {
    case object Red extends Color11
    case class Custom(rgb: String) extends Color11

    implicit val customFormatter: TypedMongoFormat[Custom] = new TypedMongoFormat[Custom] {
      override def toMongoValue(a: Custom): MongoType =
        dbObj("type" -> "Custom", "rgb" -> a.rgb, "extraField" -> "panda")
      override def fromMongoValue(any: MongoType): Custom =
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

    implicit def customFormatter[A <: Bound]: TypedMongoFormat[Custom[A]] =
      new TypedMongoFormat[Custom[A]] {
        override def toMongoValue(a: Custom[A]): MongoType =
          dbObj("type" -> "Custom", "rgb" -> a.rgb, "extraField" -> "panda")
        override def fromMongoValue(any: MongoType): Custom[A] =
          Custom(any.asInstanceOf[BSONObject].get("rgb").asInstanceOf[String])
      }

    val format = deriveMongoFormat[ColorUpperBound]
  }

  sealed trait ColorUnbound
  object ColorUnbound {
    case object Red extends ColorUnbound
    case class Custom[A](rgb: String) extends ColorUnbound

    implicit def customFormatter[A]: TypedMongoFormat[Custom[A]] = new TypedMongoFormat[Custom[A]] {
      override def toMongoValue(a: Custom[A]): MongoType =
        dbObj("type" -> "Custom", "rgb" -> a.rgb, "extraField" -> "panda")
      override def fromMongoValue(any: MongoType): Custom[A] =
        Custom(any.asInstanceOf[BSONObject].get("rgb").asInstanceOf[String])
    }

    val format = deriveMongoFormat[ColorUnbound]
  }
}