package io.sphere.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SetHandlingSpec extends AnyWordSpec with Matchers {
  "JSON deserialization" must {

    "should accept same elements in array to create a set" in {
      val jeans = getFromJSON[Jeans](
        """
          {
            "backPocket": ["mobile", "mobile"]
          }
        """)

      jeans must be(Jeans(None, None, Set("mobile"), "secret"))
    }
  }
}
