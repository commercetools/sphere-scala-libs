package io.sphere.json.generic

import io.sphere.json.JSON
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NestedSubTypeNameSpec extends AnyWordSpec with Matchers {
  import NestedSubTypeNameSpec._

  "return all subtype names in nested trait hierarchies" in {
    val format: JSON[SuperType2] =
      jsonTypeSwitch[SuperType2, SubType1, SubType2](Nil)

    val names =
      // There are 2 strange things about this list:
      // 1: Trait names are also listen, not only class names, even though we don't use those
      // 2: Class names seem to be duplicated
      // I don't think this behaviour is necessarily intentional, but it works like this at the moment.
      List("SubClass1A", "SubClass1A", "SubType1", "SubClass2A", "SubClass2A", "SubType2")
    format.asInstanceOf[TypeSelectorContainer].typeSelectors.map(_.typeValue) must be(names)
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
