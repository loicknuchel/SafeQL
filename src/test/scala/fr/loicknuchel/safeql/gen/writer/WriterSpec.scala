package fr.loicknuchel.safeql.gen.writer

import fr.loicknuchel.safeql.gen.Database
import fr.loicknuchel.safeql.gen.Database.FieldRef
import fr.loicknuchel.safeql.testingutils.FileSpec

class WriterSpec extends FileSpec {
  protected val root = "target/tests-WriterSpec"
  private val writer = ScalaWriter(now, directory = root)
  private val users = Database.Table("PUBLIC", "users", fields = List(
    Database.Field("PUBLIC", "users", "id", 4, "INTEGER", "INT NOT NULL", nullable = false, 1, None, None),
    Database.Field("PUBLIC", "users", "name", 12, "VARCHAR", "VARCHAR(50)", nullable = true, 2, None, None)))
  private val posts = Database.Table("PUBLIC", "posts", fields = List(
    Database.Field("PUBLIC", "posts", "id", 4, "INTEGER", "INT NOT NULL", nullable = false, 1, None, None),
    Database.Field("PUBLIC", "posts", "title", 12, "VARCHAR", "VARCHAR(50)", nullable = true, 2, None, None),
    Database.Field("PUBLIC", "posts", "author", 4, "INTEGER", "INT NOT NULL", nullable = false, 3, None, Some(FieldRef("PUBLIC", "users", "id")))))
  private val db = Database(schemas = List(Database.Schema("PUBLIC", tables = List(posts, users))))

  describe("Writer") {
    it("should be able to read written files") {
      writer.write(db).get
      val written = writer.readFiles().get
      val generated = writer.generateFiles(db)
      written shouldBe generated
    }
    describe("IdentifierStrategy") {
      describe("KeepNames") {
        it("should escape scala keywords") {
          val s = Writer.IdentifierStrategy.KeepNames
          s.format("val") shouldBe "`val`"
          s.format("var") shouldBe "`var`"
          s.format("def") shouldBe "`def`"
          s.format("type") shouldBe "`type`"
          s.format("class") shouldBe "`class`"
          s.format("object") shouldBe "`object`"
          s.format("import") shouldBe "`import`"
          s.format("package") shouldBe "`package`"
          s.format("other") shouldBe "other"
        }
      }
    }
  }
}
