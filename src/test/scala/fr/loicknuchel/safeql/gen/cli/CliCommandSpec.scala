package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliError.{InvalidValue, MultiError}
import fr.loicknuchel.safeql.gen.writer.ScalaWriter
import fr.loicknuchel.safeql.gen.writer.ScalaWriter.{DatabaseConfig, FieldConfig, SchemaConfig, TableConfig}
import fr.loicknuchel.safeql.gen.writer.Writer.IdentifierStrategy
import fr.loicknuchel.safeql.testingutils.BaseSpec
import pureconfig.ConfigSource

class CliCommandSpec extends BaseSpec {
  private val dbConfPath = "src/test/resources/cli/db.conf"
  private val dbConf = ScalaWriter.DatabaseConfig(schemas = Map("PUBLIC" -> ScalaWriter.SchemaConfig()))

  describe("CliCommand") {
    it("should build commands") {
      val flyway = CliConf.ReaderConf.FlywayConf(NonEmptyList.of("classpath:sql_migrations"))
      val sqlFiles = CliConf.ReaderConf.SqlFilesConf(List("db.sql"))
      val jdbc = CliConf.ReaderConf.JdbcConf("h2", "jdbc:h2:mem")
      val w1 = CliConf.WriterConf.ScalaConf(None, None, None, None)
      val w2 = CliConf.WriterConf.ScalaConf(Some("target"), Some("io.test"), Some("KeepNames"), Some(dbConfPath))

      CliCommand.from(CliConf.HelpConf()) shouldBe Right(CliCommand.Help())
      CliCommand.from(CliConf.GenConf(flyway, w1)) shouldBe Right(CliCommand.GenWithFlyway(List("classpath:sql_migrations"), ScalaWriter()))
      CliCommand.from(CliConf.GenConf(sqlFiles, w1)) shouldBe Right(CliCommand.GenWithSqlFiles(List("db.sql"), ScalaWriter()))
      CliCommand.from(CliConf.GenConf(jdbc, w1)) shouldBe Right(CliCommand.GenWithH2Jdbc("jdbc:h2:mem", ScalaWriter()))
      CliCommand.from(CliConf.GenConf(jdbc, w2)) shouldBe Right(CliCommand.GenWithH2Jdbc("jdbc:h2:mem", ScalaWriter("target", "io.test", IdentifierStrategy.KeepNames, dbConf)))

      CliCommand.from(CliConf.GenConf(jdbc, w1.copy(identifiers = Some("bad")))) shouldBe Left(CliErrors(InvalidValue("Identifier strategy", "bad")))
      CliCommand.from(CliConf.GenConf(jdbc, w1.copy(configFile = Some("bad")))) shouldBe Left(CliErrors(MultiError("Unable to read file bad (No such file or directory).")))
    }
    describe("buildIdfStrategy") {
      it("should validate identifier strategy") {
        CliCommand.buildIdfStrategy("KeepNames") shouldBe a[Right[_, _]]
        CliCommand.buildIdfStrategy("UpperCase") shouldBe a[Right[_, _]]
        CliCommand.buildIdfStrategy("NotFound") shouldBe a[Left[_, _]]
      }
    }
    describe("buildDbConf") {
      it("should buildDbConf") {
        CliCommand.buildDbConf(dbConfPath) shouldBe Right(dbConf)
      }
    }
    describe("DatabaseConfigReader") {
      it("should read database conf") {
        val r = CliCommand.DatabaseConfigReader.reader
        ConfigSource.string("").load[ScalaWriter.DatabaseConfig](r) shouldBe Right(DatabaseConfig())
        ConfigSource.string("scaladoc = test").load[ScalaWriter.DatabaseConfig](r).map(_.scaladoc(None)) shouldBe Right(DatabaseConfig(scaladoc = _ => Some("test"))).map(_.scaladoc(None))
        ConfigSource.string("imports = [a, b]").load[ScalaWriter.DatabaseConfig](r) shouldBe Right(DatabaseConfig(imports = List("a", "b")))
        ConfigSource.string("custom-types-follow-references = false").load[ScalaWriter.DatabaseConfig](r) shouldBe Right(DatabaseConfig(customTypesFollowReferences = false))
        ConfigSource.string(
          """schemas {
            |  PUBLIC {}
            |}""".stripMargin).load[ScalaWriter.DatabaseConfig](r) shouldBe
          Right(DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig())))
        ConfigSource.string(
          """schemas {
            |  PUBLIC {
            |    tables {
            |      users {}
            |    }
            |  }
            |}""".stripMargin).load[ScalaWriter.DatabaseConfig](r) shouldBe
          Right(DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map("users" -> TableConfig())))))
        ConfigSource.string(
          """schemas {
            |  PUBLIC {
            |    tables {
            |      users {
            |        alias = u
            |        sorts = [name, {slug: s, label: Score, fields: [{name: score, asc: false}]}]
            |        search = [name]
            |        fields {
            |          id {index: 1, custom-type: User.Id}
            |          name {}
            |          score {}
            |        }
            |      }
            |    }
            |  }
            |}""".stripMargin).load[ScalaWriter.DatabaseConfig](r) shouldBe
          Right(DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map("users" -> TableConfig(
            alias = Some("u"),
            sorts = List(TableConfig.Sort("name"), TableConfig.Sort("s", "Score", "-score")),
            search = List("name"),
            fields = Map(
              "id" -> FieldConfig(1, "User.Id"),
              "name" -> FieldConfig(),
              "score" -> FieldConfig())))))))
      }
    }
  }
}
