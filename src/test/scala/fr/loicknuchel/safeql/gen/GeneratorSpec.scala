package fr.loicknuchel.safeql.gen

import java.util.UUID

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.reader.H2Reader
import fr.loicknuchel.safeql.gen.writer.ScalaWriter.{DatabaseConfig, FieldConfig, SchemaConfig, TableConfig}
import fr.loicknuchel.safeql.gen.writer.{ScalaWriter, Writer}
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.safeql.utils.Extensions._
import fr.loicknuchel.safeql.utils.FileUtils
import org.flywaydb.core.Flyway
import org.flywaydb.core.internal.jdbc.DriverDataSource
import org.scalatest.BeforeAndAfterEach

import scala.util.Try

class GeneratorSpec extends BaseSpec with BeforeAndAfterEach {
  private val root = "target/tmp-generator-tests"
  private val reader = H2Reader(
    url = s"jdbc:h2:mem:${UUID.randomUUID()};MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1",
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

  override protected def afterEach(): Unit = FileUtils.delete(root).get

  describe("Generator") {
    it("should generate the same files with all the generators") {
      // Basic generation
      Flyway.configure()
        .dataSource(new DriverDataSource(this.getClass.getClassLoader, reader.driver, reader.url, reader.user, reader.pass))
        .locations("classpath:sql_migrations")
        .load().migrate()
      val basicPath = s"$root/basic-gen"
      Generator.reader(reader).writer(writer.directory(basicPath)).generate().unsafeRunSync()
      val basicDb = getFolderContent(basicPath).get

      // Flyway generator
      val flywapPath = s"$root/flyway-gen"
      Generator.flyway("classpath:sql_migrations").writer(writer.directory(flywapPath)).generate().unsafeRunSync()
      val flywayDb = getFolderContent(flywapPath).get
      flywayDb shouldBe basicDb

      // SQL files generator
      val sqlFilesPath = s"$root/sql-gen"
      Generator.fromFiles(List("src/test/resources/sql_migrations/V1__test_schema.sql")).writer(writer.directory(sqlFilesPath)).generate().unsafeRunSync()
      val sqlFilesDb = getFolderContent(sqlFilesPath).get
      sqlFilesDb shouldBe basicDb
    }
    it("should keep the generated database up to date") {
      val flywayWriter = writer.directory(s"$root/flyway-gen")
      Generator.flyway("classpath:sql_migrations").writer(flywayWriter).generate().unsafeRunSync()

      val flywayDb = getFolderContent(flywayWriter.rootFolderPath).get
      val currentDb = getFolderContent(writer.rootFolderPath).get
      currentDb shouldBe flywayDb
    }
    ignore("should generate the database tables") { // run this test to generate the test database tables
      Generator.reader(reader).writer(writer).generate().unsafeRunSync()
    }
  }

  private def getFolderContent(path: String): Try[Map[String, String]] = {
    FileUtils.listFiles(path)
      .flatMap(_.map(p => FileUtils.read(p).map(c => (p.stripPrefix(path), c))).sequence)
      .map(_.toMap)
  }
}
