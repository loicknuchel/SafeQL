package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliError.{MultiError, ParsingError}
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError.CustomError
import fr.loicknuchel.scalargs.Errs

class CliErrorSpec extends BaseSpec {
  describe("CliErrors") {
    it("should build from Errs") {
      CliErrors.from(Errs.custom("err")) shouldBe CliErrors(ParsingError(CustomError("err")))
    }
    it("should transform to list") {
      CliErrors(MultiError("e")).nel shouldBe NonEmptyList.of(MultiError("e"))
      CliErrors(MultiError("e")).toList shouldBe List(MultiError("e"))
    }
  }
}
