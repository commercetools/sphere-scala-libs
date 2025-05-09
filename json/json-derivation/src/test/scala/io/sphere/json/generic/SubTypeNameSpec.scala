package io.sphere.json.generic

import io.sphere.json.JSON
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SubTypeNameSpec extends AnyWordSpec with Matchers {
  import SubTypeNameSpec._

  "JSON.subtypeNames" must {

    val subTypeNames = List("Obj1", "Obj2", "Class1", "Class2")
    "return all subtypes objects and class of a trait when using deriveJSON" in {
      val format: JSON[SuperType] = deriveJSON

      format.subTypeNames must be(subTypeNames)

      format.asInstanceOf[TypeSelectorContainer].typeSelectors.map(_.typeValue) must be(
        subTypeNames)
    }

    "return all subtypes objects and class of a trait when using jsonTypeSwitch" in {
      implicit val obj1F: JSON[Obj1.type] = deriveJSON
      implicit val objHF: JSON[ObjHidden.type] = deriveJSON
      implicit val class1F: JSON[Class1] = deriveJSON
      implicit val classhF: JSON[ClassHidden] = deriveJSON

      val format: JSON[SuperType] =
        jsonTypeSwitch[SuperType, Obj1.type, ObjHidden.type, Class1, ClassHidden](Nil)

      format.subTypeNames must be(subTypeNames)

      format.asInstanceOf[TypeSelectorContainer].typeSelectors.map(_.typeValue) must be(
        subTypeNames)
    }
  }
}

object SubTypeNameSpec {
  sealed trait SuperType
  case object Obj1 extends SuperType
  @JSONTypeHint("Obj2") case object ObjHidden extends SuperType
  case class Class1(int: Int) extends SuperType
  @JSONTypeHint("Class2") case class ClassHidden(int: Int) extends SuperType
}
