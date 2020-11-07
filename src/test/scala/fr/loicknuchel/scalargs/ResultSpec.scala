package fr.loicknuchel.scalargs

import fr.loicknuchel.safeql.testingutils.BaseSpec

class ResultSpec extends BaseSpec {
  private val args = "a1 a2 --f1 a b --f2 c --f3"
  private val params = Params(args)
  private val p1 = params.copy(readArgs = Set(1))
  private val e1 = Errs.argNotFound(2)
  private val ok: Result[String] = Result.Success("a2", p1)
  private val err: Result[String] = Result.Failure(e1, params)

  describe("Result") {
    it("should transform values") {
      ok.map(_.length) shouldBe Result.Success(2, p1)
      err.map(_.length) shouldBe err

      ok.flatMap(_ => Result.Success(1, params)) shouldBe Result.Success(1, params)
      ok.flatMap(_ => err) shouldBe err
      err.flatMap(_ => ok) shouldBe err
    }
    it("should filter a value") {
      ok.filter(_ => true) shouldBe ok
      ok.filter(_ => false) shouldBe Result.Failure(Errs.validation("a2", None), p1)
      ok.filter(_ => false, "reason") shouldBe Result.Failure(Errs.validation("a2", Some("reason")), p1)
    }
    it("should transform value with params") {
      ok.chain((v, p) => Result.Success(v.length, p.copy(readFlags = Set("f1")))) shouldBe Result.Success(("a2", 2), p1.copy(readFlags = Set("f1")))
    }
    it("should choose the valid one and keep all errors") {
      val ok2 = ok.map(_ + "a")
      ok.orElse(ok2) shouldBe ok
      ok.orElse(err) shouldBe ok
      err.orElse(ok) shouldBe ok
      err.orElse(err) shouldBe Result.Failure(Errs.noAlternative(e1, e1), params)
    }
    it("should transform result to either") {
      ok.toEither shouldBe Right("a2" -> p1)
      err.toEither shouldBe Left(e1)
    }
  }
}
