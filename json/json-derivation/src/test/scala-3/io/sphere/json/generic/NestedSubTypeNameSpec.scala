package io.sphere.json.generic

import io.sphere.json.JSON
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NestedSubTypeNameSpec extends AnyWordSpec with Matchers {
  import NestedSubTypeNameSpec._

  "return only class names in nested trait hierarchies" in {
    val format: JSON[SuperType2] =
      jsonTypeSwitch[SuperType2, (SubType1, SubType2)]

    // Should only contain class names, no trait names
    val names = List("SubClass1A", "SubClass2A")
    format.subTypeNames must be(names)
  }
}

object NestedSubTypeNameSpec {
  sealed trait SuperType2
  sealed trait SubType1 extends SuperType2
  object SubType1 {
    case class SubClass1A(x: Int) extends SubType1
    implicit val json: JSON[SubType1] = deriveJSON
  }
  sealed trait SubType2 extends SuperType2
  object SubType2 {
    case class SubClass2A(x: Int) extends SubType2
    implicit val json: JSON[SubType2] = deriveJSON
  }
}
