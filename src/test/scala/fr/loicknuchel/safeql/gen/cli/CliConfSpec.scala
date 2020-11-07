package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliConf.ReaderConf.{FlywayConf, JdbcConf, SqlFilesConf}
import fr.loicknuchel.safeql.gen.cli.CliConf.WriterConf.ScalaConf
import fr.loicknuchel.safeql.gen.cli.CliConf.{GenConf, HelpConf}
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.Errs

class CliConfSpec extends BaseSpec {
  describe("CliConf") {
    it("should parse cli args") {
      CliConf.reader.parse("--help") shouldBe Right(HelpConf())
      CliConf.reader.parse("gen --flyway classpath:sql_migrations --output scala") shouldBe Right(GenConf(flywayConf("classpath:sql_migrations"), scalaConf()))
      CliConf.reader.parse("gen --flyway fw loc --output scala") shouldBe Right(GenConf(flywayConf("fw", "loc"), scalaConf()))
      CliConf.reader.parse("gen --sql-files V1__test_schema.sql --output scala") shouldBe Right(GenConf(SqlFilesConf(List("V1__test_schema.sql")), scalaConf()))
      CliConf.reader.parse("gen --sql-files db.sql f2 --output scala") shouldBe Right(GenConf(SqlFilesConf(List("db.sql", "f2")), scalaConf()))
      CliConf.reader.parse("gen --jdbc h2 --url jdbc:h2:mem --output scala") shouldBe Right(GenConf(JdbcConf("h2", "jdbc:h2:mem"), scalaConf()))

      CliConf.reader.parse("gen --flyway fw --output scala --dir /tmp") shouldBe Right(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"))))
      CliConf.reader.parse("gen --flyway fw --output scala --dir /tmp --package io.db") shouldBe Right(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"))))
      CliConf.reader.parse("gen --flyway fw --output scala --dir /tmp --package io.db --identifiers KeepNames") shouldBe Right(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"), Some("KeepNames"))))
      CliConf.reader.parse("gen --flyway fw --output scala --dir /tmp --package io.db --identifiers KeepNames --config dbconf.json") shouldBe Right(GenConf(flywayConf("fw"), scalaConf(Some("/tmp"), Some("io.db"), Some("KeepNames"), Some("dbconf.json"))))
    }
    it("should nicely handle errors") {
      CliConf.reader.parse("gen --flyway fw --output bad") shouldBe Left(Errs.noAlternative(Errs.custom("Unknown output 'bad'"), Errs.validation(false, Some("Use --help to see doc"))))
      // CliConf.reader.parse("gen --jdbc unknown --url jdbc:h2:mem --output scala") shouldBe Right(GenConf(JdbcConf("h2", "jdbc:h2:mem"), scalaConf()))
    }
  }

  private def flywayConf(l: String, o: String*): FlywayConf = FlywayConf(NonEmptyList.of(l, o: _*))

  private def scalaConf(directory: Option[String] = None, packageName: Option[String] = None, identifiers: Option[String] = None, configFile: Option[String] = None): ScalaConf = ScalaConf(directory, packageName, identifiers, configFile)
}
