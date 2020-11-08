package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.testingutils.BaseSpec

class ParamsSpec extends BaseSpec {
  private val args = "a1 a2 --f1 a b --f2 c --f3"
  private val params = Params(List(0 -> "a1", 1 -> "a2"), Map("f1" -> List("a", "b"), "f2" -> List("c"), "f3" -> List()), Set(), Set())

  describe("Params") {
    it("should format args into params") {
      Params(args) shouldBe params
    }
    it("should read args and mark them as read") {
      params.readArg(1) shouldBe Result.Success("a2", params.copy(readArgs = Set(1)))
      params.readArg(2) shouldBe Result.Failure(Errs.argNotFound(2), params.copy(readArgs = Set(2)))

      params.readArgOpt(1) shouldBe Result.Success(Some("a2"), params.copy(readArgs = Set(1)))
      params.readArgOpt(2) shouldBe Result.Success(None, params.copy(readArgs = Set(2)))
    }
    it("should fail when read twice an arg") {
      val r1 = params.readArg(1)
      r1.params.readArg(1) shouldBe Result.Failure(Errs.argRead(1), params.copy(readArgs = Set(1)))
    }
    it("should read flags and mark them as read") {
      params.readFlag("f2") shouldBe Result.Success("c", params.copy(readFlags = Set("f2")))
      params.readFlag("f1") shouldBe Result.Failure(Errs.multipleFlagValues("f1", List("a", "b")), params.copy(readFlags = Set("f1")))
      params.readFlag("f3") shouldBe Result.Failure(Errs.noFlagValue("f3"), params.copy(readFlags = Set("f3")))
      params.readFlag("f4") shouldBe Result.Failure(Errs.flagNotFound("f4"), params.copy(readFlags = Set("f4")))

      params.readFlagOpt("f2") shouldBe Result.Success(Some("c"), params.copy(readFlags = Set("f2")))
      params.readFlagOpt("f4") shouldBe Result.Success(None, params.copy(readFlags = Set("f4")))
      params.readFlagOpt("f1") shouldBe Result.Failure(Errs.multipleFlagValues("f1", List("a", "b")), params.copy(readFlags = Set("f1")))
      params.readFlagOpt("f3") shouldBe Result.Failure(Errs.noFlagValue("f3"), params.copy(readFlags = Set("f3")))

      params.readFlagList("f1") shouldBe Result.Success(List("a", "b"), params.copy(readFlags = Set("f1")))
      params.readFlagList("f2") shouldBe Result.Success(List("c"), params.copy(readFlags = Set("f2")))
      params.readFlagList("f3") shouldBe Result.Success(List(), params.copy(readFlags = Set("f3")))
      params.readFlagList("f4") shouldBe Result.Failure(Errs.flagNotFound("f4"), params.copy(readFlags = Set("f4")))

      params.readFlagNel("f1") shouldBe Result.Success(NonEmptyList.of("a", "b"), params.copy(readFlags = Set("f1")))
      params.readFlagNel("f2") shouldBe Result.Success(NonEmptyList.of("c"), params.copy(readFlags = Set("f2")))
      params.readFlagNel("f3") shouldBe Result.Failure(Errs.noFlagValue("f3"), params.copy(readFlags = Set("f3")))
      params.readFlagNel("f4") shouldBe Result.Failure(Errs.flagNotFound("f4"), params.copy(readFlags = Set("f4")))

      params.readFlagBool("f3") shouldBe Result.Success(true, params.copy(readFlags = Set("f3")))
      params.readFlagBool("f4") shouldBe Result.Success(false, params.copy(readFlags = Set("f4")))
      params.readFlagBool("f1") shouldBe Result.Failure(Errs.flagHasValues("f1", List("a", "b")), params.copy(readFlags = Set("f1")))
      params.readFlagBool("f2") shouldBe Result.Failure(Errs.flagHasValue("f2", "c"), params.copy(readFlags = Set("f2")))
    }
    it("should fail when read twice a flag") {
      val r1 = params.readFlag("f2")
      r1.params.readFlag("f2") shouldBe Result.Failure(Errs.flagRead("f2"), params.copy(readFlags = Set("f2")))
    }
  }
}
