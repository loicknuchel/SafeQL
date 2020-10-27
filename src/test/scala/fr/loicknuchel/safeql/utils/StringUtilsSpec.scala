package fr.loicknuchel.safeql.utils

import fr.loicknuchel.safeql.testingutils.BaseSpec

class StringUtilsSpec extends BaseSpec {
  describe("StringUtils") {
    it("should remove diacritics") {
      StringUtils.removeDiacritics("téléphone") shouldBe "telephone"
    }
    it("should transform text into slug") {
      StringUtils.slugify("L'éléphant est dehors") shouldBe "l-elephant-est-dehors"
      StringUtils.slugify("HumanTalks + Paris") shouldBe "humantalks-paris"
    }
    it("should check scala package validity") {
      StringUtils.isScalaPackage("fr.loicknuchel.safeql.utils") shouldBe true
      StringUtils.isScalaPackage("fr.loicknuchel.SafeQL.utils") shouldBe false // no maj
      StringUtils.isScalaPackage("fr.loicknuchel safeql.utils") shouldBe false // no space
      StringUtils.isScalaPackage("fr.loicknuchel.saf3ql.utils") shouldBe false // no number
      StringUtils.isScalaPackage("fr.loïcknuchel.safeql.utils") shouldBe false // no diacritics
    }
  }
}
