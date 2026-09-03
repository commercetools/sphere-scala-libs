package io.sphere.json.generic

import io.sphere.json.JSON
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

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
        jsonTypeSwitch[SuperType, Obj1.type, ObjHidden.type, Class1, ClassHidden](Nil)

      format.subTypeNames must be(subTypeNames)
    }
  }

  "JSON.subTypeName" must {

    def check(format: JSON[SuperType]): Unit = {
      format.subTypeName(classOf[Obj1.type]) must be(Some("Obj1"))
      format.subTypeName(classOf[ObjHidden.type]) must be(Some("Obj2"))
      format.subTypeName(classOf[Class1]) must be(Some("Class1"))
      format.subTypeName(classOf[ClassHidden]) must be(Some("Class2"))
    }

    "resolve each leaf subtype class to its type-hint value when using deriveJSON" in {
      check(deriveJSON)
    }

    "resolve each leaf subtype class to its type-hint value when using jsonTypeSwitch" in {
      implicit val obj1F: JSON[Obj1.type] = deriveJSON
      implicit val objHF: JSON[ObjHidden.type] = deriveJSON
      implicit val class1F: JSON[Class1] = deriveJSON
      implicit val classhF: JSON[ClassHidden] = deriveJSON

      check(jsonTypeSwitch[SuperType, Obj1.type, ObjHidden.type, Class1, ClassHidden](Nil))
    }

    "return None for a plain case class" in {
      val format: JSON[Class1] = deriveJSON
      format.subTypeName(classOf[Class1]) must be(None)
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
