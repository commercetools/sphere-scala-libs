package io.sphere.json

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.syntax.apply._
import org.json4s.JsonAST._
import io.sphere.json.field
import io.sphere.json.generic._
import io.sphere.util.Money
import org.joda.time._
import org.scalatest.matchers.must.Matchers
import org.scalatest.funspec.AnyFunSpec

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

  // case class Node(value: Option[List[Node]]) // JSON instances for recursive data types cannot be derived
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
            JField("milestones", toJValue(p.milestones)) :: Nil
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
      implicit val milestoneJSON: JSON[Milestone] = deriveJSON[Milestone]
      implicit val projectJSON: JSON[Project] = deriveJSON[Project]
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
      implicit val animalJSON: JSON[Animal] = deriveJSON
      List(Bird("Peewee"), Dog("Hasso"), Cat("Felidae")).foreach { (a: Animal) =>
        fromJSON[Animal](toJSON(a)) must equal(Valid(a))
      }
    }

    it("must provide derived instances for product types with concrete type parameters") {
      implicit val aJSON: JSON[GenericA[String]] = deriveJSON[GenericA[String]]
      val a = GenericA("hello")
      fromJSON[GenericA[String]](toJSON(a)) must equal(Valid(a))
    }

    it("must provide derived instances for product types with generic type parameters") {
      implicit def aJSON[A: FromJSON: ToJSON]: JSON[GenericA[A]] = deriveJSON[GenericA[A]]
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
      List(SingletonA, SingletonB, SingletonC).foreach { (s: SingletonEnum) =>
        fromJSON[SingletonEnum](toJSON(s)) must equal(Valid(s))
      }
    }

    it("must provide derived instances for sum types with a mix of case class / object") {
      implicit val mixedJSON: JSON[Mixed] = deriveJSON
      List(SingletonMixed, RecordMixed(1)).foreach { (m: Mixed) =>
        fromJSON[Mixed](toJSON(m)) must equal(Valid(m))
      }
    }

    it("must provide derived instances for scala.Enumeration") {
      implicit val scalaEnumJSON: JSON[ScalaEnum.Value] = jsonEnum(ScalaEnum)
      ScalaEnum.values.foreach { v =>
        val json = s"""[${toJSON(v)}]"""
        withClue(json) {
          fromJSON[Seq[ScalaEnum.Value]](json) must equal(Valid(Seq(v)))
        }
      }
    }

    it("must handle subclasses correctly in `jsonTypeSwitch`") {
      implicit val jsonImpl: JSON[TestSubjectBase] = TestSubjectBase.json

      val testSubjects = List[TestSubjectBase](
        TestSubjectConcrete1("testSubject1"),
        TestSubjectConcrete2("testSubject2"),
        TestSubjectConcrete3("testSubject3"),
        TestSubjectConcrete4("testSubject4")
      )

      testSubjects.foreach { testSubject =>
        val json = toJSON(testSubject)
        withClue(json) {
          fromJSON[TestSubjectBase](json) must equal(Valid(testSubject))
        }
      }
    }
  }

  describe("ToJSON and FromJSON") {
    it("must provide derived JSON instances for sum types") {
      // ToJSON
      implicit val birdToJSON: ToJSON[Bird] = deriveToJSON
      implicit val dogToJSON: ToJSON[Dog] = deriveToJSON
      implicit val catToJSON: ToJSON[Cat] = deriveToJSON
      implicit val animalToJSON = toJsonTypeSwitch[Animal, Bird, Dog, Cat](Nil)
      // FromJSON
      implicit val birdFromJSON: FromJSON[Bird] = deriveFromJSON
      implicit val dogFromJSON: FromJSON[Dog] = deriveFromJSON
      implicit val catFromJSON: FromJSON[Cat] = deriveFromJSON
      implicit val animalFromJSON = fromJsonTypeSwitch[Animal, Bird, Dog, Cat](Nil)

      List(Bird("Peewee"), Dog("Hasso"), Cat("Felidae")).foreach { (a: Animal) =>
        fromJSON[Animal](toJSON(a)) must equal(Valid(a))
      }
    }

    it("must provide derived instances for product types with concrete type parameters") {
      implicit val aToJSON: ToJSON[GenericA[String]] = deriveToJSON
      implicit val aFromJSON: FromJSON[GenericA[String]] = deriveFromJSON
      val a = GenericA("hello")
      fromJSON[GenericA[String]](toJSON(a)) must equal(Valid(a))
    }

    it("must provide derived instances for singleton objects") {
      implicit val toSingletonJSON = toJsonSingleton(Singleton)
      implicit val fromSingletonJSON = fromJsonSingleton(Singleton)
      val json = s"""[${toJSON(Singleton)}]"""
      withClue(json) {
        fromJSON[Seq[Singleton.type]](json) must equal(Valid(Seq(Singleton)))
      }

      // ToJSON
      implicit val toSingleAJSON = toJsonSingleton(SingletonA)
      implicit val toSingleBJSON = toJsonSingleton(SingletonB)
      implicit val toSingleCJSON = toJsonSingleton(SingletonC)
      implicit val toSingleEnumJSON =
        toJsonSingletonEnumSwitch[SingletonEnum, SingletonA.type, SingletonB.type, SingletonC.type](
          Nil)
      // FromJSON
      implicit val fromSingleAJSON = fromJsonSingleton(SingletonA)
      implicit val fromSingleBJSON = fromJsonSingleton(SingletonB)
      implicit val fromSingleCJSON = fromJsonSingleton(SingletonC)
      implicit val fromSingleEnumJSON = fromJsonSingletonEnumSwitch[
        SingletonEnum,
        SingletonA.type,
        SingletonB.type,
        SingletonC.type](Nil)

      List(SingletonA, SingletonB, SingletonC).foreach { s: SingletonEnum =>
        fromJSON[SingletonEnum](toJSON(s)) must equal(Valid(s))
      }
    }

    it("must provide derived instances for sum types with a mix of case class / object") {
      // ToJSON
      implicit val toSingleJSON: ToJSON[JSONSpec.SingletonMixed.type] = deriveToJSON
      implicit val toRecordJSON: ToJSON[RecordMixed] = deriveToJSON
      implicit val toMixedJSON = toJsonTypeSwitch[Mixed, SingletonMixed.type, RecordMixed](Nil)
      // FromJSON
      implicit val fromSingleJSON: FromJSON[JSONSpec.SingletonMixed.type] = deriveFromJSON
      implicit val fromRecordJSON: FromJSON[RecordMixed] = deriveFromJSON
      implicit val fromMixedJSON = fromJsonTypeSwitch[Mixed, SingletonMixed.type, RecordMixed](Nil)
      List(SingletonMixed, RecordMixed(1)).foreach { m: Mixed =>
        fromJSON[Mixed](toJSON(m)) must equal(Valid(m))
      }
    }

    it("must provide derived instances for scala.Enumeration") {
      implicit val toScalaEnumJSON: ToJSON[JSONSpec.ScalaEnum.Value] = toJsonEnum(ScalaEnum)
      implicit val fromScalaEnumJSON: FromJSON[JSONSpec.ScalaEnum.Value] = fromJsonEnum(ScalaEnum)
      ScalaEnum.values.foreach { v =>
        val json = s"""[${toJSON(v)}]"""
        withClue(json) {
          fromJSON[Seq[ScalaEnum.Value]](json) must equal(Valid(Seq(v)))
        }
      }
    }

    it("must handle subclasses correctly in `jsonTypeSwitch`") {
      // ToJSON
      implicit val to1: ToJSON[TestSubjectConcrete1] = deriveToJSON
      implicit val to2: ToJSON[TestSubjectConcrete2] = deriveToJSON
      implicit val to3: ToJSON[TestSubjectConcrete3] = deriveToJSON
      implicit val to4: ToJSON[TestSubjectConcrete4] = deriveToJSON
      implicit val toA =
        toJsonTypeSwitch[TestSubjectCategoryA, TestSubjectConcrete1, TestSubjectConcrete2](Nil)
      implicit val toB =
        toJsonTypeSwitch[TestSubjectCategoryB, TestSubjectConcrete3, TestSubjectConcrete4](Nil)
      implicit val toBase =
        toJsonTypeSwitch[TestSubjectBase, TestSubjectCategoryA, TestSubjectCategoryB](Nil)

      // FromJSON
      implicit val from1: FromJSON[TestSubjectConcrete1] = deriveFromJSON
      implicit val from2: FromJSON[TestSubjectConcrete2] = deriveFromJSON
      implicit val from3: FromJSON[TestSubjectConcrete3] = deriveFromJSON
      implicit val from4: FromJSON[TestSubjectConcrete4] = deriveFromJSON
      implicit val fromA =
        fromJsonTypeSwitch[TestSubjectCategoryA, TestSubjectConcrete1, TestSubjectConcrete2](Nil)
      implicit val fromB =
        fromJsonTypeSwitch[TestSubjectCategoryB, TestSubjectConcrete3, TestSubjectConcrete4](Nil)
      implicit val fromBase =
        fromJsonTypeSwitch[TestSubjectBase, TestSubjectCategoryA, TestSubjectCategoryB](Nil)

      val testSubjects = List[TestSubjectBase](
        TestSubjectConcrete1("testSubject1"),
        TestSubjectConcrete2("testSubject2"),
        TestSubjectConcrete3("testSubject3"),
        TestSubjectConcrete4("testSubject4")
      )

      testSubjects.foreach { testSubject =>
        val json = toJSON(testSubject)
        withClue(json) {
          fromJSON[TestSubjectBase](json) must equal(Valid(testSubject))
        }
      }

    }

    it("must provide derived JSON instances for product types (case classes)") {
      import JSONSpec.{Milestone, Project}
      // ToJSON
      implicit val milestoneToJSON: ToJSON[Milestone] = deriveToJSON
      implicit val projectToJSON: ToJSON[Project] = deriveToJSON
      // FromJSON
      implicit val milestoneFromJSON: FromJSON[Milestone] = deriveFromJSON
      implicit val projectFromJSON: FromJSON[Project] = deriveFromJSON

      val proj =
        Project(42, "Linux", 7, Milestone("1.0") :: Milestone("2.0") :: Milestone("3.0") :: Nil)
      fromJSON[Project](toJSON(proj)) must equal(Valid(proj))
    }
  }
}

abstract class TestSubjectBase

sealed abstract class TestSubjectCategoryA extends TestSubjectBase
sealed abstract class TestSubjectCategoryB extends TestSubjectBase

@JSONTypeHint("foo")
case class TestSubjectConcrete1(c1: String) extends TestSubjectCategoryA
case class TestSubjectConcrete2(c2: String) extends TestSubjectCategoryA

case class TestSubjectConcrete3(c3: String) extends TestSubjectCategoryB
@JSONTypeHint("foo2")
case class TestSubjectConcrete4(c4: String) extends TestSubjectCategoryB

object TestSubjectCategoryA {
  val json: JSON[TestSubjectCategoryA] = deriveJSON[TestSubjectCategoryA]
}

object TestSubjectCategoryB {
  val json: JSON[TestSubjectCategoryB] = deriveJSON[TestSubjectCategoryB]
}

object TestSubjectBase {
  val json: JSON[TestSubjectBase] = {
    implicit val jsonA: JSON[TestSubjectCategoryA] = TestSubjectCategoryA.json
    implicit val jsonB: JSON[TestSubjectCategoryB] = TestSubjectCategoryB.json

    jsonTypeSwitch[TestSubjectBase, TestSubjectCategoryA, TestSubjectCategoryB](Nil)
  }
}
