package fr.loicknuchel.safeql.gen

import java.util.UUID

import fr.loicknuchel.safeql.gen.reader.H2Reader
import fr.loicknuchel.safeql.testingutils.{BaseSpec, CLI}
import fr.loicknuchel.safeql.utils.FileUtils
import org.flywaydb.core.Flyway
import org.flywaydb.core.internal.jdbc.DriverDataSource
import org.scalatest.BeforeAndAfterEach

class GeneratorSpec extends BaseSpec with BeforeAndAfterEach {
  private val root = "target/tests-generator"

  override protected def afterEach(): Unit = FileUtils.delete(root).get

  describe("Generator") {
    it("should generate the same files with all the generators") {
      // Basic generation
      val reader = H2Reader(
        url = s"jdbc:h2:mem:${UUID.randomUUID()};MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1",
        schema = Some("PUBLIC"),
        excludes = Some(".*flyway.*"))
      Flyway.configure()
        .dataSource(new DriverDataSource(this.getClass.getClassLoader, reader.driver, reader.url, reader.user, reader.pass))
        .locations("classpath:sql_migrations")
        .load().migrate()
      val basicPath = s"$root/basic-gen"
      Generator.reader(reader).writer(CLI.GenerateSampleDatabase.writer.directory(basicPath)).generate().unsafeRunSync()
      val basicDb = FileUtils.getDirContent(basicPath).get

      // Flyway generator
      val flywapPath = s"$root/flyway-gen"
      Generator.flyway("classpath:sql_migrations").writer(CLI.GenerateSampleDatabase.writer.directory(flywapPath)).generate().unsafeRunSync()
      val flywayDb = FileUtils.getDirContent(flywapPath).get
      flywayDb shouldBe basicDb

      // SQL files generator
      val sqlFilesPath = s"$root/sql-gen"
      Generator.fromFiles(List("src/test/resources/sql_migrations/V1__test_schema.sql")).writer(CLI.GenerateSampleDatabase.writer.directory(sqlFilesPath)).generate().unsafeRunSync()
      val sqlFilesDb = FileUtils.getDirContent(sqlFilesPath).get
      sqlFilesDb shouldBe basicDb
    }
    it("should keep the generated database up to date") {
      val flywayWriter = CLI.GenerateSampleDatabase.writer.directory(s"$root/flyway-gen")
      Generator.flyway("classpath:sql_migrations").writer(flywayWriter).generate().unsafeRunSync()

      val flywayDb = FileUtils.getDirContent(flywayWriter.rootFolderPath).get
      val currentDb = FileUtils.getDirContent(CLI.GenerateSampleDatabase.writer.rootFolderPath).get
      currentDb shouldBe flywayDb
    }
  }
}
