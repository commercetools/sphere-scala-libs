package io.sphere.json.generic

import cats.data.Validated.Valid
import io.sphere.json._
import org.json4s._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonTypeHintFieldSpec extends AnyWordSpec with Matchers {
  import JsonTypeHintFieldSpec._

  "JSONTypeHintField" must {
    "allow to set another field to distinguish between types (toMongo)" in {
      val user = UserWithPicture("foo-123", Medium, "http://example.com")
      val expected = JObject(
        List(
          "userId" -> JString("foo-123"),
          "pictureSize" -> JObject(List("pictureType" -> JString("Medium"))),
          "pictureUrl" -> JString("http://example.com")))

      val json = toJValue[UserWithPicture](user)
      json must be(expected)
    }

    "allow to set another field to distinguish between types (fromMongo)" in {
      val json =
        """
        {
          "userId": "foo-123",
          "pictureSize": { "pictureType": "Medium" },
          "pictureUrl": "http://example.com"
        }
        """

      val Valid(user) = fromJSON[UserWithPicture](json)

      user must be(UserWithPicture("foo-123", Medium, "http://example.com"))
    }
    "be respected when only defined on sealed trait base-class" in {
      val expected = JObject(
        List(
          "adtEnum" -> JObject(
            "altKey" -> JString("Variation1")
          )))
        fromJValue[SealedTraitEnumTest](expected) mustBe Valid(SealedTraitEnumTest(AdtEnum.Variation1))
        toJValue[SealedTraitEnumTest](SealedTraitEnumTest(AdtEnum.Variation1)) mustBe expected
    }
    "be respected when only defined on abstract class base-class" in {
      val expected = JObject(
        List(
          "adtEnum" -> JObject(
            "altKey" -> JString("Variation1")
          )))

      fromJValue[SealedAbstractClassEnumTest](expected) mustBe Valid(SealedAbstractClassEnumTest(AdtEnum2.Variation1))
      toJValue[SealedAbstractClassEnumTest](SealedAbstractClassEnumTest(AdtEnum2.Variation1)) mustBe expected
    }
  }

}

object JsonTypeHintFieldSpec {

  @JSONTypeHintField(value = "pictureType")
  sealed trait PictureSize
  @JSONTypeHintField(value = "pictureType")
  case object Small extends PictureSize
  @JSONTypeHintField(value = "pictureType")
  case object Medium extends PictureSize
  @JSONTypeHintField(value = "pictureType")
  case object Big extends PictureSize

  object PictureSize {
    implicit val json: JSON[PictureSize] = deriveJSON[PictureSize]
  }

  case class UserWithPicture(userId: String, pictureSize: PictureSize, pictureUrl: String)

  object UserWithPicture {
    implicit val json: JSON[UserWithPicture] = jsonProduct(apply _)
  }

  // Sealed trait ADT test
  @JSONTypeHintField(value = "altKey")
  sealed trait AdtEnum

  object AdtEnum {
  case object Variation1 extends AdtEnum
    implicit val json: JSON[AdtEnum] = deriveJSON[AdtEnum]
  }

  case class SealedTraitEnumTest(adtEnum: AdtEnum)

  object SealedTraitEnumTest {
    implicit val json: JSON[SealedTraitEnumTest] = deriveJSON[SealedTraitEnumTest]
  }

  // Sealed abstract class ADT test
  @JSONTypeHintField(value = "altKey")
  sealed abstract class AdtEnum2

  object AdtEnum2 {
  case object Variation1 extends AdtEnum2
    implicit val json: JSON[AdtEnum2] = deriveJSON[AdtEnum2]
  }

  case class SealedAbstractClassEnumTest(adtEnum: AdtEnum2)

  object SealedAbstractClassEnumTest{
    implicit val json: JSON[SealedAbstractClassEnumTest] = deriveJSON[SealedAbstractClassEnumTest]
  }
}
