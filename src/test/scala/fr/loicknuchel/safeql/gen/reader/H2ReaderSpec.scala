package fr.loicknuchel.safeql.gen.reader

import doobie.syntax.connectionio._
import doobie.syntax.string._
import fr.loicknuchel.safeql.gen.Database
import fr.loicknuchel.safeql.gen.Database.FieldRef
import fr.loicknuchel.safeql.gen.reader.H2Reader.{Column, Constraint, CrossReference, Table}
import fr.loicknuchel.safeql.testingutils.BaseSpec
import org.scalatest.BeforeAndAfterAll

class H2ReaderSpec extends BaseSpec with BeforeAndAfterAll {
  private val reader = H2Reader("jdbc:h2:mem:reader_db;MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1", schema = Some("PUBLIC"))
  private val xa = reader.getTransactor

  override def beforeAll(): Unit = {
    sql"CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name VARCHAR(50))".update.run.transact(xa).unsafeRunSync()
    sql"CREATE TABLE posts (id INT NOT NULL PRIMARY KEY, title VARCHAR(50), author INT NOT NULL REFERENCES users (id))".update.run.transact(xa).unsafeRunSync()
    ()
  }

  describe("H2Reader") {
    it("should read the database schema") {
      reader.read().unsafeRunSync() shouldBe Database(schemas = List(Database.Schema("PUBLIC", tables = List(
        Database.Table("PUBLIC", "posts", fields = List(
          Database.Field("PUBLIC", "posts", "id", 4, "INTEGER", "INT NOT NULL", nullable = false, 1, None, None),
          Database.Field("PUBLIC", "posts", "title", 12, "VARCHAR", "VARCHAR(50)", nullable = true, 2, None, None),
          Database.Field("PUBLIC", "posts", "author", 4, "INTEGER", "INT NOT NULL", nullable = false, 3, None, Some(FieldRef("PUBLIC", "users", "id")))
        )),
        Database.Table("PUBLIC", "users", fields = List(
          Database.Field("PUBLIC", "users", "id", 4, "INTEGER", "INT NOT NULL", nullable = false, 1, None, None),
          Database.Field("PUBLIC", "users", "name", 12, "VARCHAR", "VARCHAR(50)", nullable = true, 2, None, None)
        ))))))
    }
    it("should exclude fields, tables and schemas using a regex") {
      reader.excludes("id|users").read().unsafeRunSync() shouldBe Database(schemas = List(Database.Schema("PUBLIC", tables = List(
        Database.Table("PUBLIC", "posts", fields = List(
          Database.Field("PUBLIC", "posts", "title", 12, "VARCHAR", "VARCHAR(50)", nullable = true, 2, None, None),
          Database.Field("PUBLIC", "posts", "author", 4, "INTEGER", "INT NOT NULL", nullable = false, 3, None, Some(FieldRef("PUBLIC", "users", "id")))
        ))))))
    }
    it("should read columns") {
      reader.readColumns(xa).unsafeRunSync() shouldBe List(
        Column("reader_db", "PUBLIC", "posts", "id", 1, None, None, None, None, false, 4, 10, 10, 10, 10, 0, None, None, None, "Unicode", "OFF", "INTEGER", false, false, 50, "", None, "", None, "INT NOT NULL", None, true),
        Column("reader_db", "PUBLIC", "posts", "title", 2, None, None, None, None, true, 12, 50, 50, 50, 10, 0, None, None, None, "Unicode", "OFF", "VARCHAR", true, false, 50, "", None, "", None, "VARCHAR(50)", None, true),
        Column("reader_db", "PUBLIC", "posts", "author", 3, None, None, None, None, false, 4, 10, 10, 10, 10, 0, None, None, None, "Unicode", "OFF", "INTEGER", false, false, 50, "", None, "", None, "INT NOT NULL", None, true),
        Column("reader_db", "PUBLIC", "users", "id", 1, None, None, None, None, false, 4, 10, 10, 10, 10, 0, None, None, None, "Unicode", "OFF", "INTEGER", false, false, 50, "", None, "", None, "INT NOT NULL", None, true),
        Column("reader_db", "PUBLIC", "users", "name", 2, None, None, None, None, true, 12, 50, 50, 50, 10, 0, None, None, None, "Unicode", "OFF", "VARCHAR", true, false, 50, "", None, "", None, "VARCHAR(50)", None, true))
    }
    it("should read constraints") {
      reader.readConstraints(xa).unsafeRunSync() shouldBe List(
        Constraint("reader_db", "PUBLIC", "CONSTRAINT_65E", "REFERENTIAL", "reader_db", "PUBLIC", "posts", "PRIMARY_KEY_6", None, "author", "", """ALTER TABLE "PUBLIC"."posts" ADD CONSTRAINT "PUBLIC"."CONSTRAINT_65E" FOREIGN KEY("author") INDEX "PUBLIC"."CONSTRAINT_INDEX_6" REFERENCES "PUBLIC"."users"("id") NOCHECK""", 11),
        Constraint("reader_db", "PUBLIC", "CONSTRAINT_65", "PRIMARY KEY", "reader_db", "PUBLIC", "posts", "PRIMARY_KEY_65", None, "id", "", """ALTER TABLE "PUBLIC"."posts" ADD CONSTRAINT "PUBLIC"."CONSTRAINT_65" PRIMARY KEY("id") INDEX "PUBLIC"."PRIMARY_KEY_65"""", 9),
        Constraint("reader_db", "PUBLIC", "CONSTRAINT_6", "PRIMARY KEY", "reader_db", "PUBLIC", "users", "PRIMARY_KEY_6", None, "id", "", """ALTER TABLE "PUBLIC"."users" ADD CONSTRAINT "PUBLIC"."CONSTRAINT_6" PRIMARY KEY("id") INDEX "PUBLIC"."PRIMARY_KEY_6"""", 6))
    }
    it("should read cross references") {
      reader.readCrossReferences(xa).unsafeRunSync() shouldBe List(
        CrossReference("reader_db", "PUBLIC", "users", "id", "reader_db", "PUBLIC", "posts", "author", 1, 1, 1, "CONSTRAINT_65E", "PRIMARY_KEY_6", 7))
    }
    it("should read tables") {
      reader.readTables(xa).unsafeRunSync() shouldBe List(
        Table("reader_db", "PUBLIC", "posts", "TABLE", "MEMORY", Some(
          """CREATE MEMORY TABLE "PUBLIC"."posts"(
            |    "id" INT NOT NULL,
            |    "title" VARCHAR(50),
            |    "author" INT NOT NULL
            |)""".stripMargin), "", 33, 7, None, "org.h2.mvstore.db.MVTable", 0),
        Table("reader_db", "PUBLIC", "users", "TABLE", "MEMORY", Some(
          """CREATE MEMORY TABLE "PUBLIC"."users"(
            |    "id" INT NOT NULL,
            |    "name" VARCHAR(50)
            |)""".stripMargin), "", 17, 4, None, "org.h2.mvstore.db.MVTable", 0)
      )
    }
  }
}
