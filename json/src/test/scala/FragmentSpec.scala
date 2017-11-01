package io.sphere.json

import io.sphere.json.generic._
import org.joda.time._
import org.json4s.JsonAST._
import org.scalatest._
import org.scalatest.MustMatchers

object FragmentSpec {
  case class Project(nr: Int, name: String, version: Int = 1, milestones: List[Milestone] = Nil)
  case class Milestone(name: String, date: Option[DateTime] = None)
  case class WebsiteInfo(@JSONKey("url") website: String, offersHttps: Boolean = false)
  case class ProjectInfo(@JSONEmbedded websiteInfo: WebsiteInfo, @JSONEmbedded embedded: Project)

  case class WithCustom(name: String, custom: JValue)
}

class FragmentSpec extends FunSpec with MustMatchers {

  describe("GetFragment") {
    it("must get fragments from simple case classes") {
      import FragmentSpec.{Project, Milestone}
      implicit val milestoneJSON = deriveJSON[Milestone]
      implicit val projectJSON = deriveJSON[Project]

      val d = DateTime.now()
      val proj = Project(42, "Linux", 7, Milestone("1.0") :: Milestone("2.0") :: Milestone("3.0", Some(d)) :: Nil)

      getFragment(proj, Seq("name")) must equal(Seq("Linux"))
      getFragment(proj, Seq("nr")) must equal(Seq(42))
      getFragment(proj, Seq("milestones")) must equal(proj.milestones)
      getFragment(proj, Seq("milestones", "name")) must equal(proj.milestones.map(_.name).toSeq)
      getFragment(proj, Seq("milestones", "date")) must equal(Seq(d))
      getFragment(proj, Seq("doesnotexist")) must equal(Seq())
      getFragment(proj, Seq("name", "doesnotexist")) must equal(Seq())
      getFragment(proj, Seq("milestones", "doesnotexist")) must equal(Seq())
    }

    it("must resolve keys and embedded objects") {
      import FragmentSpec.{Project, Milestone, ProjectInfo, WebsiteInfo}

      implicit val milestoneJSON = deriveJSON[Milestone]
      implicit val projectJSON = deriveJSON[Project]
      implicit val websiteInfoJSON = deriveJSON[WebsiteInfo]
      implicit val projectWithURLJSON = deriveJSON[ProjectInfo]

      val d = DateTime.now()
      val proj = Project(42, "Linux", 7, Milestone("1.0") :: Milestone("2.0") :: Milestone("3.0", Some(d)) :: Nil)
      val websiteInfo = WebsiteInfo("www.url.com")
      val embedded = ProjectInfo(websiteInfo, proj)

      getFragment(embedded, Seq("url")) must equal(Seq("www.url.com"))
      getFragment(embedded, Seq("offersHttps")) must equal(Seq(false))
      getFragment(embedded, Seq("name")) must equal(Seq("Linux"))
      getFragment(embedded, Seq("nr")) must equal(Seq(42))
      getFragment(embedded, Seq("milestones")) must equal(proj.milestones)
      getFragment(embedded, Seq("milestones", "name")) must equal(proj.milestones.map(_.name).toSeq)
      getFragment(embedded, Seq("milestones", "date")) must equal(Seq(d))
      getFragment(embedded, Seq("doesnotexist")) must equal(Seq())
      getFragment(embedded, Seq("name", "doesnotexist")) must equal(Seq())
      getFragment(embedded, Seq("milestones", "doesnotexist")) must equal(Seq())
    }

    it("must resolve singleton objects") {
      getFragment(Medium.asInstanceOf[PictureSize], Seq()) must equal(Seq("Medium"))

      val user = UserWithPicture("foo-123", Medium, "http://exmple.com")
      getFragment(user, Seq("userId")) must equal(Seq("foo-123"))
      getFragment(user, Seq("pictureSize")) must equal(Seq("Medium"))
    }

    it("must resolve type hints") {
      val c1: TestSubjectBase = TestSubjectConcrete1("1")
      val c2: TestSubjectBase = TestSubjectConcrete2("2")
      val c3: TestSubjectBase = TestSubjectConcrete3("3")
      val c4: TestSubjectBase = TestSubjectConcrete4("4")

      getFragment(c1, Seq("c1")) must equal(Seq("1"))
      getFragment(c1, Seq("type")) must equal(Seq("foo"))
      getFragment(c2, Seq("c2")) must equal(Seq("2"))
      getFragment(c2, Seq("type")) must equal(Seq("TestSubjectConcrete2"))
      getFragment(c3, Seq("c3")) must equal(Seq("3"))
      getFragment(c3, Seq("type")) must equal(Seq("TestSubjectConcrete3"))
      getFragment(c4, Seq("c4")) must equal(Seq("4"))
      getFragment(c4, Seq("type")) must equal(Seq("TestSubjectConcrete4"))
    }

    it("must resolve embedded json") {
      import FragmentSpec.WithCustom
      implicit val json = deriveJSON[WithCustom]

      val jobj = JObject(("int", JInt(1)), ("str", JString("hello")))
      val c1 = WithCustom("simple", JInt(1))
      val c2 = WithCustom("object", jobj)
      val c3 = WithCustom("arr", JObject(("arr", JArray(List(JInt(1), JString("hello"))))))

      getFragment(c1, Seq("custom")) must equal(Seq(1))
      getFragment(c2, Seq("custom")) must equal(Seq(jobj))
      getFragment(c2, Seq("custom", "int")) must equal(Seq(1))
      getFragment(c2, Seq("custom", "str")) must equal(Seq("hello"))
      getFragment(c3, Seq("custom", "arr")) must equal(Seq(1, "hello"))
    }
  }
}
