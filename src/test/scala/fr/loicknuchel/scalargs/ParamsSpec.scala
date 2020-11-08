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
    it("should compute the size of read params") {
      params.size shouldBe 0
      params.arg(0).size shouldBe 1
      params.arg(0, 1).size shouldBe 2
      params.flag("a").size shouldBe 1
      params.flag("a", "b").size shouldBe 2
      params.arg(0).flag("a").size shouldBe 2
    }
    it("should check if params are empty") {
      params.nonEmpty shouldBe false
      params.arg(0).nonEmpty shouldBe true
    }
    it("should read args and mark them as read") {
      params.read(a1) shouldBe Result("a2", params.arg(1))
      params.read(a2) shouldBe Result(ArgumentNotFound(2, params))

      params.read(a1o) shouldBe Result(Some("a2"), params.arg(1))
      params.read(a2o) shouldBe Result(None, params.arg(2))
    }
    it("should fail when read twice an arg") {
      val r1 = params.read(a1)
      r1.params.read(a1) shouldBe Result(ArgumentReadTwice(1, params.arg(1)))
    }
    it("should read flags and mark them as read") {
      params.read(Flag("f2")) shouldBe Result("c", params.flag("f2"))
      params.read(Flag("f1")) shouldBe Result(UniqueFlagHasMultipleValues("f1", List("a", "b"), params))
      params.read(Flag("f3")) shouldBe Result(NoFlagValue("f3", params))
      params.read(Flag("f4")) shouldBe Result(FlagNotFound("f4", params))

      params.read(FlagOpt("f2")) shouldBe Result(Some("c"), params.flag("f2"))
      params.read(FlagOpt("f4")) shouldBe Result(None, params.flag("f4"))
      params.read(FlagOpt("f1")) shouldBe Result(UniqueFlagHasMultipleValues("f1", List("a", "b"), params))
      params.read(FlagOpt("f3")) shouldBe Result(NoFlagValue("f3", params))

      params.read(FlagList("f1")) shouldBe Result(List("a", "b"), params.flag("f1"))
      params.read(FlagList("f2")) shouldBe Result(List("c"), params.flag("f2"))
      params.read(FlagList("f3")) shouldBe Result(List(), params.flag("f3"))
      params.read(FlagList("f4")) shouldBe Result(FlagNotFound("f4", params))

      params.read(FlagNel("f1")) shouldBe Result(NonEmptyList.of("a", "b"), params.flag("f1"))
      params.read(FlagNel("f2")) shouldBe Result(NonEmptyList.of("c"), params.flag("f2"))
      params.read(FlagNel("f3")) shouldBe Result(NoFlagValue("f3", params))
      params.read(FlagNel("f4")) shouldBe Result(FlagNotFound("f4", params))

      params.read(FlagBool("f3")) shouldBe Result(true, params.flag("f3"))
      params.read(FlagBool("f4")) shouldBe Result(false, params.flag("f4"))
      params.read(FlagBool("f1")) shouldBe Result(EmptyFlagHasMultipleValues("f1", List("a", "b"), params))
      params.read(FlagBool("f2")) shouldBe Result(FlagHasValue("f2", "c", params))

      params.read(FlagCheck("f3")) shouldBe Result((), params.flag("f3"))
      params.read(FlagCheck("f4")) shouldBe Result(FlagNotFound("f4", params))
      params.read(FlagCheck("f1")) shouldBe Result(EmptyFlagHasMultipleValues("f1", List("a", "b"), params))
      params.read(FlagCheck("f2")) shouldBe Result(FlagHasValue("f2", "c", params))
    }
    it("should fail when read twice a flag") {
      val r1 = params.read(Flag("f2"))
      r1.params.read(Flag("f2")) shouldBe Result(FlagReadTwice("f2", params.flag("f2")))
    }
  }
}
