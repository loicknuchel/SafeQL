package fr.loicknuchel.scalargs

import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError.{ArgumentNotFound, NoValidAlternative, ValidationError}

class ResultSpec extends BaseSpec {
  private val args = "a1 a2 --f1 a b --f2 c --f3"
  private val params = Params(args)
  private val p1 = params.arg(1)
  private val e1 = ArgumentNotFound(2, params)
  private val ok: Result[String] = Result("a2", p1)
  private val err: Result[String] = Result(e1)

  describe("Result") {
    it("should transform values") {
      ok.map(_.length) shouldBe Result(2, p1)
      err.map(_.length) shouldBe err

      ok.flatMap((_, _) => Result(1, params)) shouldBe Result(1, params)
      ok.flatMap((_, _) => err) shouldBe err
      err.flatMap((_, _) => ok) shouldBe err
    }
    it("should filter a value") {
      ok.filter(_ => true) shouldBe ok
      ok.filter(_ => false) shouldBe Result(ValidationError("a2", p1))
      ok.filter(_ => false, "reason") shouldBe Result(ValidationError("a2", "reason", p1))
    }
    it("should transform value with params") {
      ok.and(p => Result(2, p.flag("f1"))) shouldBe Result(("a2", 2), p1.flag("f1"))
      ok.chain((v, p) => Result(v.length, p.flag("f1"))) shouldBe Result(("a2", 2), p1.flag("f1"))
    }
    it("should choose the valid one and keep all errors") {
      val ok2 = ok.map(_ + "a")
      ok.orElse(ok2) shouldBe ok
      ok.orElse(err) shouldBe ok
      err.orElse(ok) shouldBe ok
      err.orElse(err) shouldBe Result(NoValidAlternative(e1, e1))
    }
    it("should transform result to either") {
      ok.toEither shouldBe Right("a2")
      err.toEither shouldBe Left(e1)
    }
  }
}
