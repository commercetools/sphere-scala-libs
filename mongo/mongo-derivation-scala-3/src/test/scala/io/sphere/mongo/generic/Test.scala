package io.sphere.mongo.generic

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Test extends AnyWordSpec with Matchers:
  case class A(x: Int)

  "asd" must {
    inspect(A(324535))
    ()
  }
  
end Test