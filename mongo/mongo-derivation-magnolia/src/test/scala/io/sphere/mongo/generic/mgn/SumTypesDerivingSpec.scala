package io.sphere.mongo.generic.mgn

import com.mongodb.DBObject
import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.format.MongoFormat
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.sphere.mongo.format.DefaultMongoFormats._

class SumTypesDerivingSpec extends AnyWordSpec with Matchers {
  import SumTypesDerivingSpec._

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

    "use custom field & values" in pendingUntilFixed {
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
      deriveMongoFormat[Color7]
    }

    "do not use sealed trait info when using a case class directly" in {
      check(Color8.format, Color8.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))

      check(Color8.Custom.format, Color8.Custom("2356"), dbObj("rgb" -> "2356"))

      // unless annotated

      check(
        Color8.format,
        Color8.CustomAnnotated("2356"),
        dbObj("type" -> "CustomAnnotated", "rgb" -> "2356"))

      check(
        Color8.CustomAnnotated.format,
        Color8.CustomAnnotated("2356"),
        dbObj("type" -> "CustomAnnotated", "rgb" -> "2356"))
    }

    "use default values if custom values are empty" in {
      check(Color9.format, Color9.Red, dbObj("type" -> "Red"))

      check(Color9.format, Color9.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))
    }
  }

}

object SumTypesDerivingSpec {
  import Matchers._

  def check[A, B <: A](format: MongoFormat[A], b: B, dbo: DBObject): Assertion = {
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
  }

  sealed trait Color8
  object Color8 {
    case object Red extends Color8
    case class Custom(rgb: String) extends Color8
    object Custom {
      val format = deriveMongoFormat[Custom]
    }
    @MongoTypeHintField("type")
    case class CustomAnnotated(rgb: String) extends Color8
    object CustomAnnotated {
      val format = deriveMongoFormat[CustomAnnotated]
    }
    val format = deriveMongoFormat[Color8]
  }

  sealed trait Color9
  object Color9 {
    @MongoTypeHint("")
    case object Red extends Color9
    @MongoTypeHint("  ")
    case class Custom(rgb: String) extends Color9
    val format = deriveMongoFormat[Color9]
  }

}
