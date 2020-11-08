package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliConf.ReaderConf.{FlywayConf, JdbcConf, SqlFilesConf}
import fr.loicknuchel.safeql.gen.cli.CliConf.WriterConf.ScalaConf
import fr.loicknuchel.safeql.gen.cli.CliConf.{GenConf, HelpConf}
import fr.loicknuchel.safeql.gen.writer.Writer.IdentifierStrategy
import fr.loicknuchel.safeql.gen.writer.Writer.IdentifierStrategy.KeepNames
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError.{ArgumentNotFound, FlagNotFound, NoValidAlternative}

class CliConfSpec extends BaseSpec {
  describe("CliConf") {
    it("should parse cli args") {
      CliConf.reader.read("--help").get shouldBe Some(HelpConf())
      CliConf.reader.read("gen --flyway classpath:sql_migrations --output scala").get shouldBe Some(GenConf(flywayConf("classpath:sql_migrations"), scalaConf()))
      CliConf.reader.read("gen --flyway fw loc --output scala").get shouldBe Some(GenConf(flywayConf("fw", "loc"), scalaConf()))
      CliConf.reader.read("gen --sql-files V1__test_schema.sql --output scala").get shouldBe Some(GenConf(SqlFilesConf(List("V1__test_schema.sql")), scalaConf()))
      CliConf.reader.read("gen --sql-files db.sql f2 --output scala").get shouldBe Some(GenConf(SqlFilesConf(List("db.sql", "f2")), scalaConf()))
      CliConf.reader.read("gen --jdbc h2 --url jdbc:h2:mem --output scala").get shouldBe Some(GenConf(JdbcConf("h2", "jdbc:h2:mem"), scalaConf()))

      CliConf.reader.read("gen --flyway fw --output scala --dir /tmp").get shouldBe Some(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"))))
      CliConf.reader.read("gen --flyway fw --output scala --dir /tmp --package io.db").get shouldBe Some(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"))))
      CliConf.reader.read("gen --flyway fw --output scala --dir /tmp --package io.db --identifiers KeepNames").get shouldBe Some(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"), Some(KeepNames))))
      CliConf.reader.read("gen --flyway fw --output scala --dir /tmp --package io.db --identifiers KeepNames --config dbconf.json").get shouldBe Some(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"), Some(KeepNames), Some("dbconf.json"))))
    }
    it("should nicely handle errors") {
      CliConf.reader.read("").err shouldBe Some(NoValidAlternative(ArgumentNotFound(0, Some("command"), Some(Set("gen"))), FlagNotFound("help")))
      CliConf.reader.read("").err.map(_.getMessage).getOrElse("") shouldBe "Invalid arguments, here are your options (fix the error you want):\n  - Missing argument 1 (command, possible values: gen)\n  - Missing flag --help"

      // TODO: do not return missing --help (because of 'gen' arg) & return missing --output (because of 'and' in genReader)
      // CliConf.reader.read("gen").err shouldBe Some(NoValidAlternative(FlagNotFound("flyway"), FlagNotFound("sql-files"), FlagNotFound("jdbc"), FlagNotFound("help")))
      CliConf.reader.read("gen").err shouldBe Some(NoValidAlternative(NoValidAlternative(FlagNotFound("flyway"), FlagNotFound("sql-files"), FlagNotFound("jdbc")), FlagNotFound("help")))

      // CliConf.reader.read("gen --flyway fw --output bad").err shouldBe Some(NoValidAlternative(CustomError("Unknown output 'bad'"), FlagNotFound("help")))
      // CliConf.reader.read("gen --jdbc unknown --url jdbc:h2:mem --output scala").get shouldBe Some(GenConf(JdbcConf("h2", "jdbc:h2:mem"), scalaConf()))
    }
  }

  private def flywayConf(l: String, o: String*): FlywayConf = FlywayConf(NonEmptyList.of(l, o: _*))

  private def scalaConf(directory: Option[String] = None, packageName: Option[String] = None, identifiers: Option[IdentifierStrategy] = None, configFile: Option[String] = None): ScalaConf = ScalaConf(directory, packageName, identifiers, configFile)
}
