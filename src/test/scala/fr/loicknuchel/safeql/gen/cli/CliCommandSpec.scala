package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliCommandSpec.{dbConf, dbConfPath}
import fr.loicknuchel.safeql.gen.cli.CliError.{InvalidValue, MultiError}
import fr.loicknuchel.safeql.gen.writer.ScalaWriter
import fr.loicknuchel.safeql.gen.writer.ScalaWriter.{DatabaseConfig, FieldConfig, SchemaConfig, TableConfig}
import fr.loicknuchel.safeql.gen.writer.Writer.IdentifierStrategy
import fr.loicknuchel.safeql.testingutils.BaseSpec
import pureconfig.ConfigSource

class CliCommandSpec extends BaseSpec {
  describe("CliCommand") {
    it("should build commands") {
      val flyway = CliConf.ReaderConf.FlywayConf(NonEmptyList.of("classpath:sql_migrations"))
      val sqlFiles = CliConf.ReaderConf.SqlFilesConf(List("db.sql"))
      val jdbc = CliConf.ReaderConf.JdbcConf("h2", "jdbc:h2:mem")
      val w1 = CliConf.WriterConf.ScalaConf(None, None, None, None)
      val w2 = CliConf.WriterConf.ScalaConf(Some("target"), Some("io.test"), Some("KeepNames"), Some(dbConfPath))

      CliCommand.from(now, CliConf.HelpConf()) shouldBe Right(CliCommand.Help())
      CliCommand.from(now, CliConf.GenConf(flyway, w1)) shouldBe Right(CliCommand.GenWithFlyway(List("classpath:sql_migrations"), ScalaWriter(now)))
      CliCommand.from(now, CliConf.GenConf(sqlFiles, w1)) shouldBe Right(CliCommand.GenWithSqlFiles(List("db.sql"), ScalaWriter(now)))
      CliCommand.from(now, CliConf.GenConf(jdbc, w1)) shouldBe Right(CliCommand.GenWithH2Jdbc("jdbc:h2:mem", ScalaWriter(now)))
      CliCommand.from(now, CliConf.GenConf(jdbc, w2)) shouldBe Right(CliCommand.GenWithH2Jdbc("jdbc:h2:mem", ScalaWriter(now, "target", "io.test", IdentifierStrategy.KeepNames, dbConf)))

      CliCommand.from(now, CliConf.GenConf(jdbc, w1.copy(identifiers = Some("bad")))) shouldBe Left(CliErrors(InvalidValue("Identifier strategy", "bad")))
      CliCommand.from(now, CliConf.GenConf(jdbc, w1.copy(configFile = Some("bad")))) shouldBe Left(CliErrors(MultiError("Unable to read file bad (No such file or directory).")))
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
            |        sorts = [
            |          id,
            |          {slug: name, label: name, fields: [name, -id]},
            |          {slug: s, label: Score, fields: [{name: score, asc: false}]}
            |        ]
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
            sorts = List(
              TableConfig.Sort("id"),
              TableConfig.Sort("name", NonEmptyList.of("name", "-id")),
              TableConfig.Sort("s", "Score", "-score")),
            search = List("name"),
            fields = Map(
              "id" -> FieldConfig(1, "User.Id"),
              "name" -> FieldConfig(),
              "score" -> FieldConfig())))))))
      }
    }
  }
}

object CliCommandSpec {
  val dbConfPath = "src/test/resources/cli/db.conf"
  val dbConf: DatabaseConfig = DatabaseConfig(
    imports = List("fr.loicknuchel.safeql.testingutils.Entities._"),
    schemas = Map("PUBLIC" -> SchemaConfig(tables = Map(
      "users" -> TableConfig(alias = Some("u"), fields = Map(
        "id" -> FieldConfig(customType = Some("User.Id")))),
      "categories" -> TableConfig(alias = "c", sort = TableConfig.Sort("name", NonEmptyList.of("-name", "id")), search = List("name"), fields = Map(
        "id" -> FieldConfig(customType = Some("Category.Id")))),
      "posts" -> TableConfig(alias = Some("p"), fields = Map(
        "id" -> FieldConfig(customType = Some("Post.Id"))))
    ))))
}
