package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliConf.ReaderConf.{FlywayConf, JdbcConf, SqlFilesConf}
import fr.loicknuchel.safeql.gen.cli.CliConf.WriterConf.ScalaConf
import fr.loicknuchel.safeql.gen.cli.CliConf.{GenConf, HelpConf}
import fr.loicknuchel.safeql.gen.writer.Writer.IdentifierStrategy
import fr.loicknuchel.safeql.gen.writer.Writer.IdentifierStrategy.KeepNames
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError._
import fr.loicknuchel.scalargs.{Params, Result}
import org.scalatest.Assertion

class CliConfSpec extends BaseSpec {
  describe("CliConf") {
    it("should parse cli args") {
      on("--help                                              ") { p => CliConf.reader.read(p) shouldBe Result(HelpConf(), p.flag("help")) }
      on("gen --flyway classpath:sql_migrations --output scala") { p => CliConf.reader.read(p) shouldBe Result(GenConf(flywayConf("classpath:sql_migrations"), scalaConf()), p.arg(0).flag("flyway", "output", "dir", "package", "identifiers", "config")) }
      on("gen --flyway fw loc                   --output scala") { p => CliConf.reader.read(p) shouldBe Result(GenConf(flywayConf("fw", "loc"), scalaConf()), p.arg(0).flag("flyway", "output", "dir", "package", "identifiers", "config")) }
      on("gen --sql-files V1__test_schema.sql   --output scala") { p => CliConf.reader.read(p) shouldBe Result(GenConf(SqlFilesConf(List("V1__test_schema.sql")), scalaConf()), p.arg(0).flag("sql-files", "output", "dir", "package", "identifiers", "config")) }
      on("gen --sql-files db.sql f2             --output scala") { p => CliConf.reader.read(p) shouldBe Result(GenConf(SqlFilesConf(List("db.sql", "f2")), scalaConf()), p.arg(0).flag("sql-files", "output", "dir", "package", "identifiers", "config")) }
      on("gen --jdbc h2 --url jdbc:h2:mem       --output scala") { p => CliConf.reader.read(p) shouldBe Result(GenConf(JdbcConf("h2", "jdbc:h2:mem"), scalaConf()), p.arg(0).flag("jdbc", "url", "output", "dir", "package", "identifiers", "config")) }

      on("gen --flyway fw --output scala --dir /tmp                                                             ") { p => CliConf.reader.read(p) shouldBe Result(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"))), p.arg(0).flag("flyway", "output", "dir", "package", "identifiers", "config")) }
      on("gen --flyway fw --output scala --dir /tmp --package io.db                                             ") { p => CliConf.reader.read(p) shouldBe Result(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"))), p.arg(0).flag("flyway", "output", "dir", "package", "identifiers", "config")) }
      on("gen --flyway fw --output scala --dir /tmp --package io.db --identifiers KeepNames                     ") { p => CliConf.reader.read(p) shouldBe Result(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"), Some(KeepNames))), p.arg(0).flag("flyway", "output", "dir", "package", "identifiers", "config")) }
      on("gen --flyway fw --output scala --dir /tmp --package io.db --identifiers KeepNames --config dbconf.json") { p => CliConf.reader.read(p) shouldBe Result(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"), Some(KeepNames), Some("dbconf.json"))), p.arg(0).flag("flyway", "output", "dir", "package", "identifiers", "config")) }
    }
    it("should show nice error when no args") {
      val p = Params("")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(ArgumentNotFound(0, "command", Set("gen"), p), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe
        """Invalid arguments, here are your options (fix the error you want):
          |  - Missing argument 1 (command), possible values: gen
          |  - Missing --help flag""".stripMargin
    }
    it("should show nice error when only 'gen' command") {
      // TODO: return missing --output (because of 'and' in genReader)
      val p = Params("gen")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(NoValidAlternative(FlagNotFound("flyway", p.arg(0)), FlagNotFound("sql-files", p.arg(0)), FlagNotFound("jdbc", p.arg(0))), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe
        """Invalid arguments, here are your options (fix the error you want):
          |  - Missing --flyway flag
          |  - Missing --sql-files flag
          |  - Missing --jdbc flag""".stripMargin
    }
    it("should show nice error when only 'gen --flyway' command") {
      val p = Params("gen --flyway")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(NoValidAlternative(NoFlagValue("flyway", p.arg(0)), FlagNotFound("sql-files", p.arg(0)), FlagNotFound("jdbc", p.arg(0))), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe
        """Invalid arguments, here are your options (fix the error you want):
          |  - Missing a value for --flyway flag
          |  - Missing --sql-files flag
          |  - Missing --jdbc flag""".stripMargin
    }
    it("should show nice error when only 'gen --flyway fw' command") {
      val p = Params("gen --flyway fw")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(FlagNotFound("output", p.arg(0).flag("flyway")), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe "Missing --output flag"
    }
    it("should show nice error when only 'gen --flyway fw --output' command") {
      val p = Params("gen --flyway fw --output")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(NoFlagValue("output", p.arg(0).flag("flyway")), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe "Missing a value for --output flag"
    }
    it("should show nice error when only 'gen --flyway fw --output bad' command") {
      val p = Params("gen --flyway fw --output bad")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(CustomError("Unknown output 'bad'", p.arg(0).flag("flyway", "output")), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe "Unknown output 'bad'"
    }
    it("should show nice error when only 'gen --flyway fw --output scala --dir' command") {
      val p = Params("gen --flyway fw --output scala --dir")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(NoFlagValue("dir", p.arg(0).flag("flyway", "output")), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe "Missing a value for --dir flag"
    }
    it("should show nice error when only 'gen --flyway fw --output scala --dir a b' command") {
      val p = Params("gen --flyway fw --output scala --dir a b")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(UniqueFlagHasMultipleValues("dir", List("a", "b"), p.arg(0).flag("flyway", "output")), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe "--dir flag has multiple values (a, b) but expects only one"
    }
    it("should show nice error when only 'gen --flyway fw --output scala --identifiers bad' command") {
      val p = Params("gen --flyway fw --output scala --identifiers bad")
      CliConf.reader.read(p) shouldBe Result(NoValidAlternative(InvalidEnumValue("bad", Set("KeepNames", "UpperCase"), p.arg(0).flag("flyway", "output", "dir", "package", "identifiers")), FlagNotFound("help", p)))
      CliConf.reader.read(p).toEither.fold(_.getMessage, _ => "") shouldBe "Value 'bad' is not allowed, possible values: KeepNames, UpperCase"
    }
    // TODO --sql-files errors
    // TODO --jdbc errors
  }

  private def flywayConf(l: String, o: String*): FlywayConf = FlywayConf(NonEmptyList.of(l, o: _*))

  private def scalaConf(directory: Option[String] = None, packageName: Option[String] = None, identifiers: Option[IdentifierStrategy] = None, configFile: Option[String] = None): ScalaConf = ScalaConf(directory, packageName, identifiers, configFile)

  private def on(a: String)(f: Params => Assertion): Assertion = f(Params(a.trim))
}
