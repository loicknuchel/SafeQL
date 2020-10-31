package fr.loicknuchel.safeql.testingutils

import cats.data.NonEmptyList
import cats.effect.IO
import fr.loicknuchel.safeql.gen.Generator
import fr.loicknuchel.safeql.gen.writer.ScalaWriter.{DatabaseConfig, FieldConfig, SchemaConfig, TableConfig}
import fr.loicknuchel.safeql.gen.writer.{ScalaWriter, Writer}

object CLI {
  def main(args: Array[String]): Unit = {
    GenerateSampleDatabase.run().unsafeRunSync()
    println("Done")
  }

  object GenerateSampleDatabase {
    val writer: ScalaWriter = ScalaWriter(
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

    def run(): IO[Unit] = {
      Generator.flyway("classpath:sql_migrations").writer(writer).generate()
    }
  }

}
