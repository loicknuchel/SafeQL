package fr.loicknuchel.scalargs

import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError.{ArgumentNotFound, FlagNotFound, NoValidAlternative}

class ArgErrorSpec extends BaseSpec {
  describe("ArgError") {
    it("should format ArgumentNotFound") {
      ArgumentNotFound(0).getMessage shouldBe "Missing argument 1"
      ArgumentNotFound(0, Some("command")).getMessage shouldBe "Missing argument 1 (command)"
      ArgumentNotFound(0, Some("command"), Some(Set("gen", "check"))).getMessage shouldBe "Missing argument 1 (command, possible values: gen, check)"
      ArgumentNotFound(0, values = Some(Set("gen", "check"))).getMessage shouldBe "Missing argument 1 (possible values: gen, check)"
    }
    it("should format FlagNotFound") {
      FlagNotFound("f1").getMessage shouldBe "Missing flag --f1"
    }
    it("should format NoValidAlternative") {
      NoValidAlternative(ArgumentNotFound(0), FlagNotFound("f1")).getMessage shouldBe
        """Invalid arguments, here are your options (fix the error you want):
          |  - Missing argument 1
          |  - Missing flag --f1""".stripMargin
    }
    it("should add many errors to NoValidAlternative at once") {
      val (e1, e2, e3, e4) = (ArgumentNotFound(1), ArgumentNotFound(2), ArgumentNotFound(3), ArgumentNotFound(4))
      NoValidAlternative(e1, e2, e3, e4) shouldBe NoValidAlternative(e1, e2, List(e3, e4))
    }
  }
}
