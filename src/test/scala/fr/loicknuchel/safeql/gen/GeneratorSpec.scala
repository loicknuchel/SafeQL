package fr.loicknuchel.safeql.gen

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.reader.H2Reader
import fr.loicknuchel.safeql.gen.writer.ScalaWriter.{DatabaseConfig, FieldConfig, SchemaConfig, TableConfig}
import fr.loicknuchel.safeql.gen.writer.{ScalaWriter, Writer}
import fr.loicknuchel.safeql.testingutils.SqlSpec

class GeneratorSpec extends SqlSpec {
  private val reader = H2Reader(
    url = dbUrl,
    user = dbUser,
    pass = dbPass,
    schema = Some("PUBLIC"),
    excludes = Some(".*flyway.*"))
  private val writer = ScalaWriter(
    directory = "src/test/scala",
    packageName = "fr.loicknuchel.safeql.testingutils.database",
    identifierStrategy = Writer.IdentifierStrategy.upperCase,
    config = DatabaseConfig(
      scaladoc = _ => Some("Hello"),
      imports = List("fr.loicknuchel.safeql.testingutils.Entities._"),
      schemas = Map("PUBLIC" -> SchemaConfig(tables = Map(
        "users" -> TableConfig(alias = Some("u"), fields = Map(
          "id" -> FieldConfig(customType = Some("User.Id")))),
        "categories" -> TableConfig(alias = "c", sort = TableConfig.Sort("name", NonEmptyList.of("-name", "id")), search = List("name"), fields = Map(
          "id" -> FieldConfig(customType = Some("Category.Id")))),
        "posts" -> TableConfig(alias = Some("p"), fields = Map(
          "id" -> FieldConfig(customType = Some("Post.Id"))))
      )))))

  describe("Generator") {
    ignore("should generate database tables") {
      Generator.generate(reader, writer).unsafeRunSync()
    }
    it("should generate same files as before") {
      val existingFiles = writer.readFiles().get
      val database = reader.read().unsafeRunSync()
      val newFiles = writer.generateFiles(database)
      newFiles.size shouldBe existingFiles.size
      newFiles.map { case (path, content) =>
        content.trim shouldBe existingFiles.getOrElse(path, "").trim
      }
    }
  }
}
