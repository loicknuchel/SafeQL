package fr.loicknuchel.scalargs

import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError.{ArgumentNotFound, FlagNotFound, NoFlagValue, NoValidAlternative}

class ArgErrorSpec extends BaseSpec {
  private val p = Params("")

  describe("ArgError") {
    it("should format ArgumentNotFound") {
      ArgumentNotFound(0, p).getMessage shouldBe "Missing argument 1"
      ArgumentNotFound(0, "command", p).getMessage shouldBe "Missing argument 1 (command)"
      ArgumentNotFound(0, "command", Set("gen", "check"), p).getMessage shouldBe "Missing argument 1 (command), possible values: gen, check"
      ArgumentNotFound(0, Set("gen", "check"), p).getMessage shouldBe "Missing argument 1, possible values: gen, check"
    }
    it("should format FlagNotFound") {
      FlagNotFound("f1", p).getMessage shouldBe "Missing --f1 flag"
    }
    it("should format NoFlagValue") {
      NoFlagValue("f1", p).getMessage shouldBe "Missing a value for --f1 flag"
    }
    describe("NoValidAlternative") {
      it("should add many errors at once") {
        val (e1, e2, e3, e4) = (FlagNotFound("f1", p), FlagNotFound("f2", p), FlagNotFound("f3", p), FlagNotFound("f4", p))
        NoValidAlternative(e1, e2, e3, e4) shouldBe NoValidAlternative(e1, e2, List(e3, e4), p)
      }
      it("should use the biggest param as its own") {
        NoValidAlternative(FlagNotFound("f1", p), FlagNotFound("f2", p.arg(0, 1)), FlagNotFound("f3", p.arg(2))).params shouldBe p.arg(0, 1)
      }
      it("should display only the deepest messages") {
        NoValidAlternative(FlagNotFound("f1", p), FlagNotFound("f2", p), FlagNotFound("f3", p), FlagNotFound("f4", p)).getMessage shouldBe
          """Invalid arguments, here are your options (fix the error you want):
            |  - Missing --f1 flag
            |  - Missing --f2 flag
            |  - Missing --f3 flag
            |  - Missing --f4 flag""".stripMargin

        NoValidAlternative(FlagNotFound("f1", p), FlagNotFound("f2", p.arg(0)), FlagNotFound("f3", p.arg(0)), FlagNotFound("f4", p.arg(0))).getMessage shouldBe
          """Invalid arguments, here are your options (fix the error you want):
            |  - Missing --f2 flag
            |  - Missing --f3 flag
            |  - Missing --f4 flag""".stripMargin

        NoValidAlternative(FlagNotFound("f1", p), FlagNotFound("f2", p.arg(0)), FlagNotFound("f3", p.arg(0, 1)), FlagNotFound("f4", p.arg(0, 1))).getMessage shouldBe
          """Invalid arguments, here are your options (fix the error you want):
            |  - Missing --f3 flag
            |  - Missing --f4 flag""".stripMargin
      }
      it("should not display a list when only one message is displayable") {
        NoValidAlternative(FlagNotFound("f1", p), FlagNotFound("f2", p.arg(0))).getMessage shouldBe "Missing --f2 flag"
      }
      it("should correctly indent lists") {
        NoValidAlternative(FlagNotFound("f1", p), NoValidAlternative(FlagNotFound("f2", p), FlagNotFound("f3", p)), FlagNotFound("f4", p)).getMessage shouldBe
          """Invalid arguments, here are your options (fix the error you want):
            |  - Missing --f1 flag
            |  - Invalid arguments, here are your options (fix the error you want):
            |    - Missing --f2 flag
            |    - Missing --f3 flag
            |  - Missing --f4 flag""".stripMargin
      }
    }
  }
}
