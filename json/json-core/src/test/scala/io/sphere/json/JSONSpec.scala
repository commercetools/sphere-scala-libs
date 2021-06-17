package io.sphere.json

import org.json4s.JsonAST.JValue
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JSONSpec extends AnyWordSpec with Matchers {
  import JSONSpec._
  "JSON.apply" must {
    "find possible JSON instance" in {
      implicit val testJson: JSON[Test] = new JSON[Test] {
        override def read(jval: JValue): JValidation[Test] = ???
        override def write(value: Test): JValue = ???
      }
      JSON[Test] must be(testJson)
    }
    "create instance from FromJSON and ToJSON" in {
      JSON[Int]
      JSON[List[Double]]
      JSON[Map[String, Int]]
    }
  }
}

object JSONSpec {
  case class Test(a: String)
}
