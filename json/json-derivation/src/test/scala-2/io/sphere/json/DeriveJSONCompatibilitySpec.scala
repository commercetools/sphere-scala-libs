package io.sphere.json

import io.sphere.json.generic.{deriveJSON, jsonProduct}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.UUID

class DeriveJSONCompatibilitySpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  "jsonProduct must work the same as deriveJSON for case classes" in {

    import DeriveJSONCompatibilitySpec._

    val deriveSyntax: JSON[ClassOfManyTypes] = {
      implicit val nestedF: JSON[NestedClass] = deriveJSON
      deriveJSON
    }
    val jsonProductSyntax: JSON[ClassOfManyTypes] = {
      implicit val nestedF: JSON[NestedClass] = jsonProduct(NestedClass.apply _)
      jsonProduct(ClassOfManyTypes.apply _)
    }

    forAll { (x: ClassOfManyTypes) =>
      val jsonJP = jsonProductSyntax.write(x)
      val jsonD = deriveSyntax.write(x)

      jsonJP must be(jsonD)

      jsonProductSyntax.read(jsonD).getOrElse(null) must be(x)
      deriveSyntax.read(jsonJP).getOrElse(null) must be(x)
    }

  }

}

object DeriveJSONCompatibilitySpec {

  case class NestedClass(id: UUID, str2: String, int2: Int)

  case class ClassOfManyTypes(
      str: String,
      int: Int,
      long: Long,
      list: List[Int],
      mapOfNested: Map[String, NestedClass]
  )

  implicit val userArbitrary: Arbitrary[NestedClass] = Arbitrary {
    for {
      id <- Gen.const(UUID.randomUUID())
      firstName <- Gen.alphaStr.suchThat(_.nonEmpty)
      age <- Gen.chooseNum(1, 120)
    } yield NestedClass(id, firstName, age)
  }

  implicit val cmtArbitrary: Arbitrary[ClassOfManyTypes] = Arbitrary {
    for {
      str <- Gen.alphaStr.suchThat(_.nonEmpty)
      int <- Arbitrary.arbitrary[Int]
      long <- Arbitrary.arbitrary[Long]
      list <- Gen.listOf(Arbitrary.arbitrary[Int])
      map <- Gen.mapOf(Gen.zip(Gen.alphaStr.suchThat(_.nonEmpty), Arbitrary.arbitrary[NestedClass]))
    } yield ClassOfManyTypes(str, int, long, list, map)
  }
}
