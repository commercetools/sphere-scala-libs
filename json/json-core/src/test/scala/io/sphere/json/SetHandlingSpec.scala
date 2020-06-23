package io.sphere.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SetHandlingSpec extends AnyWordSpec with Matchers {
  "JSON deserialization" must {

    "should accept same elements in array to create a set" in {
      val jeans = getFromJSON[Set[String]](
        """
           ["mobile", "mobile"]
        """)

      jeans must be(Set("mobile"))
    }
  }
}
