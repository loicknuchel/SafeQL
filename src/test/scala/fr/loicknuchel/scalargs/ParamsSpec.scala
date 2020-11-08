package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError._
import fr.loicknuchel.scalargs.Reader.{Flag, FlagBool, FlagCheck, FlagList, FlagNel, FlagOpt}

class ParamsSpec extends BaseSpec {
  private val args = "a1 a2 --f1 a b --f2 c --f3"
  private val params = Params(List(0 -> "a1", 1 -> "a2"), Map("f1" -> List("a", "b"), "f2" -> List("c"), "f3" -> List()), Set(), Set())
  private val (a1, a2) = (Reader.arg(1), Reader.arg(2))
  private val (a1o, a2o) = (Reader.argOpt(1), Reader.argOpt(2))

  describe("Params") {
    it("should format args into params") {
      Params(args) shouldBe params
    }
    it("should read args and mark them as read") {
      params.read(a1) shouldBe Result.Success("a2", params.copy(readArgs = Set(1)))
      params.read(a2) shouldBe Result.Failure(ArgumentNotFound(2), params.copy(readArgs = Set(2)))

      params.read(a1o) shouldBe Result.Success(Some("a2"), params.copy(readArgs = Set(1)))
      params.read(a2o) shouldBe Result.Success(None, params.copy(readArgs = Set(2)))
    }
    it("should fail when read twice an arg") {
      val r1 = params.read(a1)
      r1.params.read(a1) shouldBe Result.Failure(ArgumentReadTwice(1), params.copy(readArgs = Set(1)))
    }
    it("should read flags and mark them as read") {
      params.read(Flag("f2")) shouldBe Result.Success("c", params.copy(readFlags = Set("f2")))
      params.read(Flag("f1")) shouldBe Result.Failure(UniqueFlagHasMultipleValues("f1", List("a", "b")), params.copy(readFlags = Set("f1")))
      params.read(Flag("f3")) shouldBe Result.Failure(NoFlagValue("f3"), params.copy(readFlags = Set("f3")))
      params.read(Flag("f4")) shouldBe Result.Failure(FlagNotFound("f4"), params.copy(readFlags = Set("f4")))

      params.read(FlagOpt("f2")) shouldBe Result.Success(Some("c"), params.copy(readFlags = Set("f2")))
      params.read(FlagOpt("f4")) shouldBe Result.Success(None, params.copy(readFlags = Set("f4")))
      params.read(FlagOpt("f1")) shouldBe Result.Failure(UniqueFlagHasMultipleValues("f1", List("a", "b")), params.copy(readFlags = Set("f1")))
      params.read(FlagOpt("f3")) shouldBe Result.Failure(NoFlagValue("f3"), params.copy(readFlags = Set("f3")))

      params.read(FlagList("f1")) shouldBe Result.Success(List("a", "b"), params.copy(readFlags = Set("f1")))
      params.read(FlagList("f2")) shouldBe Result.Success(List("c"), params.copy(readFlags = Set("f2")))
      params.read(FlagList("f3")) shouldBe Result.Success(List(), params.copy(readFlags = Set("f3")))
      params.read(FlagList("f4")) shouldBe Result.Failure(FlagNotFound("f4"), params.copy(readFlags = Set("f4")))

      params.read(FlagNel("f1")) shouldBe Result.Success(NonEmptyList.of("a", "b"), params.copy(readFlags = Set("f1")))
      params.read(FlagNel("f2")) shouldBe Result.Success(NonEmptyList.of("c"), params.copy(readFlags = Set("f2")))
      params.read(FlagNel("f3")) shouldBe Result.Failure(NoFlagValue("f3"), params.copy(readFlags = Set("f3")))
      params.read(FlagNel("f4")) shouldBe Result.Failure(FlagNotFound("f4"), params.copy(readFlags = Set("f4")))

      params.read(FlagBool("f3")) shouldBe Result.Success(true, params.copy(readFlags = Set("f3")))
      params.read(FlagBool("f4")) shouldBe Result.Success(false, params.copy(readFlags = Set("f4")))
      params.read(FlagBool("f1")) shouldBe Result.Failure(EmptyFlagHasMultipleValues("f1", List("a", "b")), params.copy(readFlags = Set("f1")))
      params.read(FlagBool("f2")) shouldBe Result.Failure(FlagHasValue("f2", "c"), params.copy(readFlags = Set("f2")))

      params.read(FlagCheck("f3")) shouldBe Result.Success((), params.copy(readFlags = Set("f3")))
      params.read(FlagCheck("f4")) shouldBe Result.Failure(FlagNotFound("f4"), params.copy(readFlags = Set("f4")))
      params.read(FlagCheck("f1")) shouldBe Result.Failure(EmptyFlagHasMultipleValues("f1", List("a", "b")), params.copy(readFlags = Set("f1")))
      params.read(FlagCheck("f2")) shouldBe Result.Failure(FlagHasValue("f2", "c"), params.copy(readFlags = Set("f2")))
    }
    it("should fail when read twice a flag") {
      val r1 = params.read(Flag("f2"))
      r1.params.read(Flag("f2")) shouldBe Result.Failure(FlagReadTwice("f2"), params.copy(readFlags = Set("f2")))
    }
  }
}
