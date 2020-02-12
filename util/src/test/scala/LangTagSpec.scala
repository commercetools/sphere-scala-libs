package io.sphere.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

import scala.language.postfixOps

class LangTagSpec extends AnyFunSpec with Matchers {
  describe("LangTag") {
    it("should accept valid language tags") {
      LangTag.unapply("de").isEmpty must be (false)
      LangTag.unapply("fr").isEmpty must be (false)
      LangTag.unapply("de-DE").isEmpty must be (false)
      LangTag.unapply("de-AT").isEmpty must be (false)
      LangTag.unapply("de-CH").isEmpty must be (false)
      LangTag.unapply("fr-FR").isEmpty must be (false)
      LangTag.unapply("fr-CA").isEmpty must be (false)
      LangTag.unapply("he-IL-u-ca-hebrew-tz-jeruslm").isEmpty must be (false)
    }

    it("should not accept invalid language tags") {
      LangTag.unapply(" de").isEmpty must be (true)
      LangTag.unapply("de_DE").isEmpty must be (true)
      LangTag.unapply("e-DE").isEmpty must be (true)
    }
  }
}
