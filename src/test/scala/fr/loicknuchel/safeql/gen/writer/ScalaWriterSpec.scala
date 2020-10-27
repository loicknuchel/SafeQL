package fr.loicknuchel.safeql.gen.writer

import fr.loicknuchel.safeql.gen.Database
import fr.loicknuchel.safeql.gen.Database.FieldRef
import fr.loicknuchel.safeql.gen.writer.ScalaWriter.TableConfig.Sort
import fr.loicknuchel.safeql.gen.writer.ScalaWriter.{DatabaseConfig, FieldConfig, SchemaConfig, TableConfig}
import fr.loicknuchel.safeql.testingutils.BaseSpec

class ScalaWriterSpec extends BaseSpec {
  private val users = Database.Table("PUBLIC", "users", fields = List(
    Database.Field("PUBLIC", "users", "id", 4, "INTEGER", "INT NOT NULL", nullable = false, 1, None, None),
    Database.Field("PUBLIC", "users", "name", 12, "VARCHAR", "VARCHAR(50)", nullable = true, 2, None, None)))
  private val posts = Database.Table("PUBLIC", "posts", fields = List(
    Database.Field("PUBLIC", "posts", "id", 4, "INTEGER", "INT NOT NULL", nullable = false, 1, None, None),
    Database.Field("PUBLIC", "posts", "title", 12, "VARCHAR", "VARCHAR(50)", nullable = true, 2, None, None),
    Database.Field("PUBLIC", "posts", "author", 4, "INTEGER", "INT NOT NULL", nullable = false, 3, None, Some(FieldRef("PUBLIC", "users", "id")))))
  private val db = Database(schemas = List(Database.Schema("PUBLIC", tables = List(posts, users))))
  private val writer = new ScalaWriter()

  describe("ScalaWriter") {
    it("should build needed paths") {
      writer.rootFolderPath shouldBe "src/main/scala/safeql"
      writer.listTablesFilePath shouldBe "src/main/scala/safeql/Tables.scala"
      writer.tablesFolderPath shouldBe "src/main/scala/safeql/tables"
      writer.tableFilePath(users) shouldBe "src/main/scala/safeql/tables/USERS.scala"
    }
    describe("list template") {
      it("should generate a table attribute") {
        writer.listTableAttr(users) shouldBe "val USERS: tables.USERS = tables.USERS.table"
      }
    }
    describe("table template") {
      describe("table imports") {
        it("should generate basic imports") {
          writer.tableImports(users, TableConfig(), List()) shouldBe
            "import fr.loicknuchel.safeql.Table._\nimport fr.loicknuchel.safeql._"
        }
        it("should add global imports when defined") {
          writer.tableImports(users, TableConfig(), List("java.time.Instant")) shouldBe
            "import java.time.Instant\n\nimport fr.loicknuchel.safeql.Table._\nimport fr.loicknuchel.safeql._"
        }
        it("should add NonEmptyList when there is sorts") {
          writer.tableImports(users, TableConfig(sorts = List(Sort("name"))), List()) shouldBe
            "import cats.data.NonEmptyList\nimport fr.loicknuchel.safeql.Table._\nimport fr.loicknuchel.safeql._"
        }
      }
      describe("field attribute") {
        it("should generate a field attribute") {
          writer.tableFieldAttr(users, users.fields.head, DatabaseConfig()) shouldBe
            "val ID: SqlField[Int, USERS] = SqlField(this, \"id\", \"INT NOT NULL\", JdbcType.Integer, nullable = false, 1)"
        }
        it("should put the custom type when defined") {
          val conf = DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map("users" -> TableConfig(fields = Map("id" -> FieldConfig("User.Id")))))))
          writer.tableFieldAttr(users, users.fields.head, conf) shouldBe
            "val ID: SqlField[User.Id, USERS] = SqlField(this, \"id\", \"INT NOT NULL\", JdbcType.Integer, nullable = false, 1)"
        }
        it("should generate a reference field attribute") {
          writer.tableFieldAttr(posts, posts.fields(2), DatabaseConfig()) shouldBe
            "val AUTHOR: SqlFieldRef[Int, POSTS, USERS] = SqlField(this, \"author\", \"INT NOT NULL\", JdbcType.Integer, nullable = false, 3, USERS.table.ID)"
        }
        it("should put custom type on reference type") {
          val conf = DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map("posts" -> TableConfig(fields = Map("author" -> FieldConfig("User.Id")))))))
          writer.tableFieldAttr(posts, posts.fields(2), conf) shouldBe
            "val AUTHOR: SqlFieldRef[User.Id, POSTS, USERS] = SqlField(this, \"author\", \"INT NOT NULL\", JdbcType.Integer, nullable = false, 3, USERS.table.ID)"
        }
        it("should use the referenced field to set a custom type") {
          val conf = DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map("users" -> TableConfig(fields = Map("id" -> FieldConfig("User.Id")))))))
          writer.tableFieldAttr(posts, posts.fields(2), conf) shouldBe
            "val AUTHOR: SqlFieldRef[User.Id, POSTS, USERS] = SqlField(this, \"author\", \"INT NOT NULL\", JdbcType.Integer, nullable = false, 3, USERS.table.ID)"
        }
        it("should be able to disable following referenced field for custom type") {
          val conf = DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map("users" -> TableConfig(fields = Map("id" -> FieldConfig("User.Id")))))), customTypesFollowReferences = false)
          writer.tableFieldAttr(posts, posts.fields(2), conf) shouldBe
            "val AUTHOR: SqlFieldRef[Int, POSTS, USERS] = SqlField(this, \"author\", \"INT NOT NULL\", JdbcType.Integer, nullable = false, 3, USERS.table.ID)"
        }
        it("should prefer direct custom type than reference one when both are available") {
          val conf = DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map(
            "users" -> TableConfig(fields = Map("id" -> FieldConfig("User.Ref"))),
            "posts" -> TableConfig(fields = Map("author" -> FieldConfig("User.Id")))))))
          writer.tableFieldAttr(posts, posts.fields(2), conf) shouldBe
            "val AUTHOR: SqlFieldRef[User.Id, POSTS, USERS] = SqlField(this, \"author\", \"INT NOT NULL\", JdbcType.Integer, nullable = false, 3, USERS.table.ID)"
        }
        it("should be able to reference field in the same table") {
          writer.tableFieldAttr(posts, posts.fields(2).copy(ref = Some(FieldRef("PUBLIC", "posts", "id"))), DatabaseConfig()) shouldBe
            "val AUTHOR: SqlFieldRef[Int, POSTS, POSTS] = SqlField(this, \"author\", \"INT NOT NULL\", JdbcType.Integer, nullable = false, 3, ID)"
        }
      }
      describe("sort") {
        it("should build a sort") {
          writer.tableSort(Sort("id")) shouldBe "Sort(\"id\", \"id\", NonEmptyList.of(ID.asc))"
          writer.tableSort(Sort("-id")) shouldBe "Sort(\"id\", \"id\", NonEmptyList.of(ID.desc))"
        }
      }
    }
    describe("scaladoc") {
      it("should build the default scaladoc") {
        writer.writeScaladoc(None, DatabaseConfig()) shouldBe "/**\n * Class generated by fr.loicknuchel.safeql.gen.writer.ScalaWriter\n */"
      }
      it("should add specific scaladoc") {
        writer.writeScaladoc(None, DatabaseConfig(scaladoc = _ => Some("My custom scaladoc"))) shouldBe "/**\n * My custom scaladoc\n *\n * Class generated by fr.loicknuchel.safeql.gen.writer.ScalaWriter\n */"
      }
    }
    describe("DatabaseConfig") {
      it("should forbid duplicated table alias") {
        DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map(
          "users" -> TableConfig(alias = "u"),
          "conf" -> TableConfig(alias = "u")
        )))).getConfigErrors shouldBe List("Alias 'u' can't be used for multiple tables (PUBLIC.users, PUBLIC.conf)")
      }
      it("should forbid schema not present in db") {
        DatabaseConfig(schemas = Map(
          "NotFound" -> SchemaConfig()
        )).getDatabaseErrors(db) shouldBe List("Schema 'NotFound' declared in conf does not exist in Database")
      }
      it("should forbid table not present in db") {
        DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map(
          "NotFound" -> TableConfig()
        )))).getDatabaseErrors(db) shouldBe List("Table 'PUBLIC.NotFound' declared in conf does not exist in Database")
      }
      it("should forbid field not present in db") {
        DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map(
          "users" -> TableConfig(fields = Map(
            "NotFound" -> FieldConfig()
          )))))).getDatabaseErrors(db) shouldBe List("Field 'PUBLIC.users.NotFound' declared in conf does not exist in Database")
      }
      it("should forbid table sort using field not present in db") {
        DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map(
          "users" -> TableConfig(sorts = List(TableConfig.Sort("name", "NotFound")))
        )))).getDatabaseErrors(db) shouldBe List("Field 'NotFound' in sort 'name' of table 'PUBLIC.users' does not exist in Database")
      }
      it("should forbid table search using field not present in db") {
        DatabaseConfig(schemas = Map("PUBLIC" -> SchemaConfig(tables = Map(
          "users" -> TableConfig(search = List("NotFound"))
        )))).getDatabaseErrors(db) shouldBe List("Field 'NotFound' in search of table 'PUBLIC.users' does not exist in Database")
      }
    }
  }
}
