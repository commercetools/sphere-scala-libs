package io.sphere.json

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.syntax.apply.*
import org.json4s.JsonAST.*
import io.sphere.json.field
import io.sphere.json.generic.*
import io.sphere.util.Money
import org.joda.time.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.funspec.AnyFunSpec
import org.json4s.DefaultJsonFormats.given

object JSONSpec {
  case class Test(a: String)

  case class Project(nr: Int, name: String, version: Int = 1, milestones: List[Milestone] = Nil)
  case class Milestone(name: String, date: Option[DateTime] = None)

  sealed abstract class Animal
  case class Dog(name: String) extends Animal
  case class Cat(name: String) extends Animal
  case class Bird(name: String) extends Animal

  sealed trait GenericBase[A]
  case class GenericA[A](a: A) extends GenericBase[A]
  case class GenericB[A](a: A) extends GenericBase[A]

  case object Singleton

  sealed abstract class SingletonEnum
  case object SingletonA extends SingletonEnum
  case object SingletonB extends SingletonEnum
  case object SingletonC extends SingletonEnum

  sealed trait Mixed
  case object SingletonMixed extends Mixed
  case class RecordMixed(i: Int) extends Mixed

  object ScalaEnum extends Enumeration {
    val One, Two, Three = Value
  }

  // JSON instances for recursive data types cannot be derived
  case class Node(value: Option[List[Node]])
}

class JSONSpec extends AnyFunSpec with Matchers {
  import JSONSpec._

  describe("JSON.apply") {
    it("must find possible JSON instance") {
      implicit val testJson: JSON[Test] = new JSON[Test] {
        override def read(jval: JValue): JValidation[Test] = ???
        override def write(value: Test): JValue = ???
      }

      JSON[Test] must be(testJson)
    }

    it("must create instance from FromJSON and ToJSON") {
      JSON[Int]
      JSON[List[Double]]
      JSON[Map[String, Int]]
    }
  }

  describe("JSON") {
    it("must read/write a custom class using custom typeclass instances") {
      import JSONSpec.{Milestone, Project}

      implicit object MilestoneJSON extends JSON[Milestone] {
        def write(m: Milestone): JValue = JObject(
          JField("name", JString(m.name)) ::
            JField("date", toJValue(m.date)) :: Nil
        )
        def read(j: JValue): ValidatedNel[JSONError, Milestone] = j match {
          case o: JObject =>
            (field[String]("name")(o), field[Option[DateTime]]("date")(o)).mapN(Milestone.apply)
          case _ => fail("JSON object expected.")
        }
      }
      implicit object ProjectJSON extends JSON[Project] {
        def write(p: Project): JValue = JObject(
          JField("nr", JInt(p.nr)) ::
            JField("name", JString(p.name)) ::
            JField("version", JInt(p.version)) ::
            JField("milestones", toJValue(p.milestones)) ::
            Nil
        )
        def read(jval: JValue): ValidatedNel[JSONError, Project] = jval match {
          case o: JObject =>
            (
              field[Int]("nr")(o),
              field[String]("name")(o),
              field[Int]("version", Some(1))(o),
              field[List[Milestone]]("milestones", Some(Nil))(o)).mapN(Project.apply)
          case _ => fail("JSON object expected.")
        }
      }

      val proj = Project(42, "Linux")
      fromJSON[Project](toJSON(proj)) must equal(Valid(proj))

      // Now some invalid JSON to test the error accumulation
      val wrongTypeJSON = """
      {
        "nr":"1",
        "name":23,
        "version":1,
        "milestones":[{"name":"Bravo", "date": "xxx"}]
      }
      """

      val Invalid(errs) = fromJSON[Project](wrongTypeJSON): @unchecked
      errs.toList must equal(
        List(
          JSONFieldError(List("nr"), "JSON Number in the range of an Int expected."),
          JSONFieldError(List("name"), "JSON String expected."),
          JSONFieldError(List("milestones", "date"), "Failed to parse date/time: xxx")
        ))

      // Now without a version value and without a milestones list. Defaults should apply.
      val noVersionJSON = """{"nr":1,"name":"Linux"}"""
      fromJSON[Project](noVersionJSON) must equal(Valid(Project(1, "Linux")))
    }

    it("must fail reading wrong currency code.") {
      val wrongMoney = """{"currencyCode":"WRONG","centAmount":1000}"""
      fromJSON[Money](wrongMoney).isInvalid must be(true)
    }

    it("must provide derived JSON instances for product types (case classes)") {
      import JSONSpec.{Milestone, Project}
      given JSON[Milestone] = deriveJSON[Milestone]
      given JSON[Project] = deriveJSON[Project]
      val proj =
        Project(42, "Linux", 7, Milestone("1.0") :: Milestone("2.0") :: Milestone("3.0") :: Nil)
      fromJSON[Project](toJSON(proj)) must equal(Valid(proj))
    }

    it("must handle empty String") {
      val Invalid(err) = fromJSON[Int](""): @unchecked
      err.toList.head mustBe a[JSONParseError]
    }

    it("must provide user-friendly error by empty String") {
      val Invalid(err) = fromJSON[Int](""): @unchecked
      err.toList mustEqual List(JSONParseError("No content to map due to end-of-input"))
    }

    it("must handle incorrect json") {
      val Invalid(err) = fromJSON[Int]("""{"key: "value"}"""): @unchecked
      err.toList.head mustBe a[JSONParseError]
    }

    it("must provide user-friendly error by incorrect json") {
      val Invalid(err) = fromJSON[Int]("""{"key: "value"}"""): @unchecked
      err.toList mustEqual List(JSONParseError(
        "Unexpected character ('v' (code 118)): was expecting a colon to separate field name and value"))
    }

    it("must provide derived JSON instances for sum types") {
      given JSON[Animal] = deriveJSON
      List(Bird("Peewee"), Dog("Hasso"), Cat("Felidae")).foreach { animal =>
        fromJSON[Animal](toJSON(animal)) must equal(Valid(animal))
      }
    }

    it("must provide derived instances for product types with concrete type parameters") {
      given JSON[GenericA[String]] = deriveJSON[GenericA[String]]
      val a = GenericA("hello")
      fromJSON[GenericA[String]](toJSON(a)) must equal(Valid(a))
    }

    it("must provide derived instances for product types with generic type parameters") {
      implicit def aJSON[A: JSON]: JSON[GenericA[A]] = deriveJSON[GenericA[A]]
      val a = GenericA("hello")
      fromJSON[GenericA[String]](toJSON(a)) must equal(Valid(a))
    }

    it("must provide derived instances for singleton objects") {
      implicit val singletonJSON: JSON[JSONSpec.Singleton.type] =
        deriveJSON[JSONSpec.Singleton.type]

      val json = s"""[${toJSON(Singleton)}]"""
      withClue(json) {
        fromJSON[Seq[Singleton.type]](json) must equal(Valid(Seq(Singleton)))
      }

      implicit val singleEnumJSON: JSON[SingletonEnum] = deriveJSON[SingletonEnum]
      List(SingletonA, SingletonB, SingletonC).foreach { s =>
        fromJSON[SingletonEnum](toJSON(s)) must equal(Valid(s))
      }
    }

    it("must provide derived instances for sum types with a mix of case class / object") {
      given JSON[Mixed] = deriveJSON
      List(SingletonMixed, RecordMixed(1)).foreach { m =>
        fromJSON[Mixed](toJSON(m)) must equal(Valid(m))
      }
    }
//
//    it("must provide derived instances for scala.Enumeration") {
//      import io.sphere.json.generic.JSON.derived
//      implicit val scalaEnumJSON: JSON[JSONSpec.ScalaEnum.Value] = deriveJSON[ScalaEnum.Value]
//      ScalaEnum.values.foreach { v =>
//        val json = s"""[${toJSON(v)}]"""
//        withClue(json) {
//          fromJSON[Seq[ScalaEnum.Value]](json) must equal(Valid(Seq(v)))
//        }
//      }
//    }
//
//    it("must handle subclasses correctly in `jsonTypeSwitch`") {
//      implicit val jsonImpl = TestSubjectBase.json
//
//      val testSubjects = List[TestSubjectBase](
//        TestSubjectConcrete1("testSubject1"),
//        TestSubjectConcrete2("testSubject2"),
//        TestSubjectConcrete3("testSubject3"),
//        TestSubjectConcrete4("testSubject4")
//      )
//
//      testSubjects.foreach { testSubject =>
//        val json = toJSON(testSubject)
//        withClue(json) {
//          fromJSON[TestSubjectBase](json) must equal(Valid(testSubject))
//        }
//      }
//
//    }
//
//  }
//
//  describe("ToJSON and FromJSON") {
//    it("must provide derived JSON instances for sum types") {
//      // ToJSON
//      implicit val birdToJSON = deriveJSON[Bird].write(Bird.apply _)
//      implicit val dogToJSON = deriveJSON[Dog].write(Dog.apply _)
//      implicit val catToJSON = toJsonProduct(Cat.apply _)
//      implicit val animalToJSON = toJsonTypeSwitch[Animal, Bird, Dog, Cat](Nil)
//      // FromJSON
//      implicit val birdFromJSON = fromJsonProduct(Bird.apply _)
//      implicit val dogFromJSON = fromJsonProduct(Dog.apply _)
//      implicit val catFromJSON = fromJsonProduct(Cat.apply _)
//      implicit val animalFromJSON = fromJsonTypeSwitch[Animal, Bird, Dog, Cat](Nil)
//
//      List(Bird("Peewee"), Dog("Hasso"), Cat("Felidae")).foreach {
//        a: Animal =>
//          fromJSON[Animal](toJSON(a)) must equal(Valid(a))
//      }
//    }
//
//    it("must provide derived instances for product types with concrete type parameters") {
//      implicit val aToJSON = toJsonProduct(GenericA.apply[String] _)
//      implicit val aFromJSON = fromJsonProduct(GenericA.apply[String] _)
//      val a = GenericA("hello")
//      fromJSON[GenericA[String]](toJSON(a)) must equal(Valid(a))
//    }
//
//    it("must provide derived instances for singleton objects") {
//      implicit val toSingletonJSON = toJsonSingleton(Singleton)
//      implicit val fromSingletonJSON = fromJsonSingleton(Singleton)
//      val json = s"""[${toJSON(Singleton)}]"""
//      withClue(json) {
//        fromJSON[Seq[Singleton.type]](json) must equal(Valid(Seq(Singleton)))
//      }
//
//      // ToJSON
//      implicit val toSingleAJSON = toJsonSingleton(SingletonA)
//      implicit val toSingleBJSON = toJsonSingleton(SingletonB)
//      implicit val toSingleCJSON = toJsonSingleton(SingletonC)
//      implicit val toSingleEnumJSON =
//        toJsonSingletonEnumSwitch[SingletonEnum, SingletonA.type, SingletonB.type, SingletonC.type](
//          Nil)
//      // FromJSON
//      implicit val fromSingleAJSON = fromJsonSingleton(SingletonA)
//      implicit val fromSingleBJSON = fromJsonSingleton(SingletonB)
//      implicit val fromSingleCJSON = fromJsonSingleton(SingletonC)
//      implicit val fromSingleEnumJSON = fromJsonSingletonEnumSwitch[
//        SingletonEnum,
//        SingletonA.type,
//        SingletonB.type,
//        SingletonC.type](Nil)
//
//      List(SingletonA, SingletonB, SingletonC).foreach {
//        s: SingletonEnum =>
//          fromJSON[SingletonEnum](toJSON(s)) must equal(Valid(s))
//      }
//    }
//
//    it("must provide derived instances for sum types with a mix of case class / object") {
//      // ToJSON
//      implicit val toSingleJSON = toJsonProduct0(SingletonMixed)
//      implicit val toRecordJSON = toJsonProduct(RecordMixed.apply _)
//      implicit val toMixedJSON = toJsonTypeSwitch[Mixed, SingletonMixed.type, RecordMixed](Nil)
//      // FromJSON
//      implicit val fromSingleJSON = fromJsonProduct0(SingletonMixed)
//      implicit val fromRecordJSON = fromJsonProduct(RecordMixed.apply _)
//      implicit val fromMixedJSON = fromJsonTypeSwitch[Mixed, SingletonMixed.type, RecordMixed](Nil)
//      List(SingletonMixed, RecordMixed(1)).foreach {
//        m: Mixed =>
//          fromJSON[Mixed](toJSON(m)) must equal(Valid(m))
//      }
//    }
//
//    it("must provide derived instances for scala.Enumeration") {
//      implicit val toScalaEnumJSON = toJsonEnum(ScalaEnum)
//      implicit val fromScalaEnumJSON = fromJsonEnum(ScalaEnum)
//      ScalaEnum.values.foreach { v =>
//        val json = s"""[${toJSON(v)}]"""
//        withClue(json) {
//          fromJSON[Seq[ScalaEnum.Value]](json) must equal(Valid(Seq(v)))
//        }
//      }
//    }
//
//    it("must handle subclasses correctly in `jsonTypeSwitch`") {
//      // ToJSON
//      implicit val to1 = toJsonProduct(TestSubjectConcrete1.apply _)
//      implicit val to2 = toJsonProduct(TestSubjectConcrete2.apply _)
//      implicit val to3 = toJsonProduct(TestSubjectConcrete3.apply _)
//      implicit val to4 = toJsonProduct(TestSubjectConcrete4.apply _)
//      implicit val toA =
//        toJsonTypeSwitch[TestSubjectCategoryA, TestSubjectConcrete1, TestSubjectConcrete2](Nil)
//      implicit val toB =
//        toJsonTypeSwitch[TestSubjectCategoryB, TestSubjectConcrete3, TestSubjectConcrete4](Nil)
//      implicit val toBase =
//        toJsonTypeSwitch[TestSubjectBase, TestSubjectCategoryA, TestSubjectCategoryB](Nil)
//
//      // FromJSON
//      implicit val from1 = fromJsonProduct(TestSubjectConcrete1.apply _)
//      implicit val from2 = fromJsonProduct(TestSubjectConcrete2.apply _)
//      implicit val from3 = fromJsonProduct(TestSubjectConcrete3.apply _)
//      implicit val from4 = fromJsonProduct(TestSubjectConcrete4.apply _)
//      implicit val fromA =
//        fromJsonTypeSwitch[TestSubjectCategoryA, TestSubjectConcrete1, TestSubjectConcrete2](Nil)
//      implicit val fromB =
//        fromJsonTypeSwitch[TestSubjectCategoryB, TestSubjectConcrete3, TestSubjectConcrete4](Nil)
//      implicit val fromBase =
//        fromJsonTypeSwitch[TestSubjectBase, TestSubjectCategoryA, TestSubjectCategoryB](Nil)
//
//      val testSubjects = List[TestSubjectBase](
//        TestSubjectConcrete1("testSubject1"),
//        TestSubjectConcrete2("testSubject2"),
//        TestSubjectConcrete3("testSubject3"),
//        TestSubjectConcrete4("testSubject4")
//      )
//
//      testSubjects.foreach { testSubject =>
//        val json = toJSON(testSubject)
//        withClue(json) {
//          fromJSON[TestSubjectBase](json) must equal(Valid(testSubject))
//        }
//      }
//
//    }
//
//    it("must provide derived JSON instances for product types (case classes)") {
//      import JSONSpec.{Milestone, Project}
//      // ToJSON
//      implicit val milestoneToJSON = toJsonProduct(Milestone.apply _)
//      implicit val projectToJSON = toJsonProduct(Project.apply _)
//      // FromJSON
//      implicit val milestoneFromJSON = fromJsonProduct(Milestone.apply _)
//      implicit val projectFromJSON = fromJsonProduct(Project.apply _)
//
//      val proj =
//        Project(42, "Linux", 7, Milestone("1.0") :: Milestone("2.0") :: Milestone("3.0") :: Nil)
//      fromJSON[Project](toJSON(proj)) must equal(Valid(proj))
//    }
  }
}

abstract class TestSubjectBase

sealed abstract class TestSubjectCategoryA extends TestSubjectBase
sealed abstract class TestSubjectCategoryB extends TestSubjectBase

@JSONTypeHint("foo")
case class TestSubjectConcrete1(c1: String) extends TestSubjectCategoryA
case class TestSubjectConcrete2(c2: String) extends TestSubjectCategoryA

case class TestSubjectConcrete3(c3: String) extends TestSubjectCategoryB
case class TestSubjectConcrete4(c4: String) extends TestSubjectCategoryB

object TestSubjectCategoryA {

  val json: JSON[TestSubjectCategoryA] = deriveJSON[TestSubjectCategoryA]
}

object TestSubjectCategoryB {

  val json: JSON[TestSubjectCategoryB] = deriveJSON[TestSubjectCategoryB]
}

//object TestSubjectBase {
//  val json: JSON[TestSubjectBase] = {
//    implicit val jsonA = TestSubjectCategoryA.json
//    implicit val jsonB = TestSubjectCategoryB.json
//
//    jsonTypeSwitch[TestSubjectBase, (TestSubjectCategoryA, TestSubjectCategoryB)]()
//  }
//}
