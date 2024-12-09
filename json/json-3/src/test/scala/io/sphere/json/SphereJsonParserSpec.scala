package io.sphere.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SphereJsonParserSpec extends AnyWordSpec with Matchers {
  "Object mapper" must {

    "accept strings with 20_000_000 bytes" in {
      SphereJsonParser.mapper.getFactory.streamReadConstraints().getMaxStringLength must be(
        20000000)
    }
  }
}
