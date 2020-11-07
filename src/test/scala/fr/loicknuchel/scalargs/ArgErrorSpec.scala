package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError.{ArgumentNotFound, CustomError, NoValidAlternative}

class ArgErrorSpec extends BaseSpec {
  describe("ArgError") {
    describe("noAlternative") {
      it("should unwrap NoValidAlternative to flatten them") {
        val (e1, e2, e3, e4) = (ArgumentNotFound(1), ArgumentNotFound(2), ArgumentNotFound(3), ArgumentNotFound(4))
        val a12 = NoValidAlternative(e1, e2, List())
        val a34 = NoValidAlternative(e3, e4, List())
        Errs.noAlternative(Errs(e1), Errs(e2)) shouldBe Errs(a12)
        Errs.noAlternative(Errs(a12), Errs(e3)) shouldBe Errs(NoValidAlternative(e1, e2, List(e3)))
        Errs.noAlternative(Errs(e2), Errs(a34)) shouldBe Errs(NoValidAlternative(e2, e3, List(e4)))
        Errs.noAlternative(Errs(a12), Errs(a34)) shouldBe Errs(NoValidAlternative(e1, e2, List(e3, e4)))
      }
      it("should add many errs") {
        val (e1, e2, e3, e4) = (ArgumentNotFound(1), ArgumentNotFound(2), ArgumentNotFound(3), ArgumentNotFound(4))
        Errs.noAlternative(Errs(e1), Errs(e2), Errs(e3), Errs(e4)) shouldBe Errs(NoValidAlternative(e1, e2, List(e3, e4)))
      }
    }
  }
  describe("Errs") {
    it("should transform to list") {
      Errs.custom("e").nel shouldBe NonEmptyList.of(CustomError("e"))
      Errs.custom("e").toList shouldBe List(CustomError("e"))
    }
  }
}
