package io.sphere.mongo.generic

import com.mongodb.DBObject
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.format.MongoFormat
import org.bson.BSONObject
import org.scalatest.Assertion

class SumTypesDerivingSpec extends AnyWordSpec with Matchers {
  import SumTypesDerivingSpec._

  "Serializing sum types" must {
    "use 'type' as default field" in {
      check(Color1.format, Color1.Red, dbObj("type" -> "Red"))

      check(Color1.format, Color1.Custom("2356"), dbObj("type" -> "Custom", "rgb" -> "2356"))
    }

    "use custom values" in {
      check(Color3.format, Color3.Red, dbObj("type" -> "red"))

      check(Color3.format, Color3.Custom("2356"), dbObj("type" -> "custom", "rgb" -> "2356"))
    }

    "use intermediate level" in {
      check(Color7.format, Color7.Red, dbObj("type" -> "Red"))
      check(Color7.format, Color7.Blue, dbObj("type" -> "Blue"))
      check(Color7.format, Color7.Custom("234"), dbObj("rgb" -> "234", "type" -> "Custom"))
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
    case object Blue extends Color1
    val format = deriveMongoFormat[Color1]
  }

  sealed trait Color3
  object Color3 {
    @MongoTypeHint("red") case object Red extends Color3
    @MongoTypeHint("custom") case class Custom(rgb: String) extends Color3
    val format = deriveMongoFormat[Color3]
  }

  sealed trait Color7
  sealed trait Color7a extends Color7
  object Color7 {
    case object Red extends Color7a
    case class Custom(rgb: String) extends Color7a
    case object Blue extends Color7
    def format = deriveMongoFormat[Color7]
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
