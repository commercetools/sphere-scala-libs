package io.sphere.json

import io.sphere.json.generic._
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.data.Validated.Valid
import io.sphere.json.AnotherFileExample._
class MultiFileJSONSpec extends AnyWordSpec with Matchers with OptionValues {
  "deriveJSON" should {
    "handle classes in another file correctly" in {
      implicit val bugJSON = deriveJSON[Bug]
      val a = Ant("C. ligniperda")
      fromJSON[Bug](toJSON[Bug](a)) must equal(Valid(a))
    }
  }
}
