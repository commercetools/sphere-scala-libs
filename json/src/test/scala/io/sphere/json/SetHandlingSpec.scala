package io.sphere.json

import org.scalatest.{MustMatchers, WordSpec}

class SetHandlingSpec extends WordSpec with MustMatchers {
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
