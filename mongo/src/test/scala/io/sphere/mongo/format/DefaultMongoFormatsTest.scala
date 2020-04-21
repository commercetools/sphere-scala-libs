package io.sphere.mongo.format

import java.util.Locale
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.generic._
import io.sphere.util.LangTag
import org.bson.BasicBSONObject
import org.bson.types.BasicBSONList
import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.JavaConverters._

object DefaultMongoFormatsTest {
  case class User(name: String)
  object User {
    implicit val mongo: MongoFormat[User] = mongoProduct(apply _)
  }
}

class DefaultMongoFormatsTest extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  import DefaultMongoFormatsTest._

  "DefaultMongoFormats" must {
    "support List[String]" in {
      val format = listFormat[String]
      val list = Gen.listOf(Gen.alphaNumStr)

      forAll(list) { l =>
        val dbo = format.toMongoValue(l)
        dbo.asInstanceOf[BasicBSONList].asScala.toList must be (l)
        val resultList = format.fromMongoValue(dbo)
        resultList must be (l)
      }
    }

    "support List[A: MongoFormat]" in {
      val format = listFormat[User]
      val list = Gen.listOf(Gen.alphaNumStr.map(User.apply))

      check(list, format)
    }

    "support Vector[String]" in {
      val format = vecFormat[String]
      val vector = Gen.listOf(Gen.alphaNumStr).map(_.toVector)

      forAll(vector) { v =>
        val dbo = format.toMongoValue(v)
        dbo.asInstanceOf[BasicBSONList].asScala.toVector must be (v)
        val resultVector = format.fromMongoValue(dbo)
        resultVector must be (v)
      }
    }

    "support Vector[A: MongoFormat]" in {
      val format = vecFormat[User]
      val vector = Gen.listOf(Gen.alphaNumStr.map(User.apply)).map(_.toVector)

      check(vector, format)
    }

    "support Set[String]" in {
      val format = setFormat[String]
      val set = Gen.listOf(Gen.alphaNumStr).map(_.toSet)

      forAll(set) { s =>
        val dbo = format.toMongoValue(s)
        dbo.asInstanceOf[BasicBSONList].asScala.toSet must be (s)
        val resultSet = format.fromMongoValue(dbo)
        resultSet must be (s)
      }
    }

    "support Set[A: MongoFormat]" in {
      val format = setFormat[User]
      val set = Gen.listOf(Gen.alphaNumStr.map(User.apply)).map(_.toSet)

      check(set, format)
    }

    "support Map[String, String]" in {
      val format = mapFormat[String]
      val map = Gen.listOf {
        for {
          key <- Gen.alphaNumStr
          value <- Gen.alphaNumStr
        } yield (key, value)
      }.map(_.toMap)

      forAll(map) { m =>
        val dbo = format.toMongoValue(m)
        dbo.asInstanceOf[BasicBSONObject].asScala must be (m)
        val resultMap = format.fromMongoValue(dbo)
        resultMap must be (m)
      }
    }

    "support Map[String, A: MongoFormat]" in {
      val format = mapFormat[User]
      val map = Gen.listOf {
        for {
          key <- Gen.alphaNumStr
          value <- Gen.alphaNumStr.map(User.apply)
        } yield (key, value)
      }.map(_.toMap)

      check(map, format)
    }

    "support Java Locale" in {
      Locale.getAvailableLocales.filter(_.toLanguageTag != LangTag.UNDEFINED).foreach{ l: Locale =>
        localeFormat.fromMongoValue(localeFormat.toMongoValue(l)).toLanguageTag must be (l.toLanguageTag)
      }
    }
  }

  private def check[A](gen: Gen[A], format: MongoFormat[A]) = {
    forAll(gen) { value =>
      val dbo = format.toMongoValue(value)
      val result = format.fromMongoValue(dbo)
      result must be (value)
    }
  }
}
