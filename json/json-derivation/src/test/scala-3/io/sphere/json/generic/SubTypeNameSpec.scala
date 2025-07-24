package io.sphere.json.generic

import io.sphere.json.JSON
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.sphere.json.generic.deriveJSON

class SubTypeNameSpec extends AnyWordSpec with Matchers {
  import SubTypeNameSpec._

  "JSON.subtypeNames" must {

    val subTypeNames = List("Obj1", "Obj2", "Class1", "Class2")
    "return all subtypes of a trait when using deriveJSON" in {
      val format: JSON[SuperType] = deriveJSON

      format.subTypeNames must be(subTypeNames)

    }

    "return all subtypes of a trait when using jsonTypeSwitch" in {
      implicit val obj1F: JSON[Obj1.type] = deriveJSON
      implicit val objHF: JSON[ObjHidden.type] = deriveJSON
      implicit val class1F: JSON[Class1] = deriveJSON
      implicit val classhF: JSON[ClassHidden] = deriveJSON

      val format: JSON[SuperType] =
        jsonTypeSwitch[SuperType, (Obj1.type, ObjHidden.type, Class1, ClassHidden)]

      format.subTypeNames must be(subTypeNames)

    }

    "return only class names in nested trait hierarchies" in {
      val format: JSON[SuperType2] =
        jsonTypeSwitch[SuperType2, (SubType1, SubType2)]

      val names =
        List("SubClass1A", "SubClass2A", "SubType1", "SubType2")
      format.subTypeNames must be(names)
    }
  }
}

object SubTypeNameSpec {
  sealed trait SuperType
  case object Obj1 extends SuperType
  @JSONTypeHint("Obj2") case object ObjHidden extends SuperType
  case class Class1(int: Int) extends SuperType
  @JSONTypeHint("Class2") case class ClassHidden(int: Int) extends SuperType

  sealed trait SuperType2
  sealed trait SubType1 extends SuperType2
  object SubType1 {
    case class SubClass1A(x: Int) extends SubType1
    given JSON[SubType1] = deriveJSON
  }
  sealed trait SubType2 extends SuperType2
  object SubType2 {
    case class SubClass2A(x: Int) extends SubType2
    given JSON[SubType2] = deriveJSON
  }
}
