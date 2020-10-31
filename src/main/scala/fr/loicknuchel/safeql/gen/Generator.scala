package fr.loicknuchel.safeql.gen

import java.util.UUID

import cats.effect.IO
import doobie.Update0
import doobie.syntax.connectionio._
import fr.loicknuchel.safeql.gen.reader.{H2Reader, Reader}
import fr.loicknuchel.safeql.gen.writer.Writer
import fr.loicknuchel.safeql.models.FailedScript
import fr.loicknuchel.safeql.utils.Extensions._
import fr.loicknuchel.safeql.utils.FileUtils
import org.flywaydb.core.Flyway
import org.flywaydb.core.internal.jdbc.DriverDataSource

import scala.util.control.NonFatal

object Generator {
  /**
   * Flyway Generator
   */

  def flyway(flywayLocations: String*): FlywayGeneratorBuilder = {
    val reader = H2Reader(
      url = s"jdbc:h2:mem:${UUID.randomUUID()};MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1",
      schema = Some("PUBLIC"),
      excludes = Some(".*flyway.*"))
    val flyway = Flyway.configure()
      .dataSource(new DriverDataSource(this.getClass.getClassLoader, reader.driver, reader.url, reader.user, reader.pass))
      .locations(flywayLocations: _*)
      .load()
    new FlywayGeneratorBuilder(flyway, reader)
  }

  class FlywayGeneratorBuilder(flyway: Flyway, reader: H2Reader) {
    def writer(writer: Writer): FlywayGenerator = new FlywayGenerator(flyway, reader, writer)

    def excludes(regex: String): FlywayGeneratorBuilder = new FlywayGeneratorBuilder(flyway, reader.excludes(regex))
  }

  class FlywayGenerator(flyway: Flyway, reader: H2Reader, writer: Writer) {
    def generate(): IO[Unit] = IO(flyway.migrate()).flatMap(_ => Generator.generate(reader, writer))

    def excludes(regex: String): FlywayGenerator = new FlywayGenerator(flyway, reader.excludes(regex), writer)
  }

  /**
   * SQL files Generator
   */

  def fromFiles(paths: List[String]): SQLFilesGeneratorBuilder = {
    val reader = H2Reader(
      url = s"jdbc:h2:mem:${UUID.randomUUID()};MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1",
      schema = Some("PUBLIC"),
      excludes = None)
    new SQLFilesGeneratorBuilder(paths, reader)
  }

  class SQLFilesGeneratorBuilder(paths: List[String], reader: H2Reader) {
    def writer(writer: Writer): SQLFilesGenerator = new SQLFilesGenerator(paths, reader, writer)

    def excludes(regex: String): SQLFilesGeneratorBuilder = new SQLFilesGeneratorBuilder(paths, reader.excludes(regex))
  }

  class SQLFilesGenerator(paths: List[String], reader: H2Reader, writer: Writer) {
    def generate(): IO[Unit] = for {
      files <- paths.map(FileUtils.read).sequence.toIO
      _ <- files.map(exec(_, reader.xa)).sequence
      _ <- Generator.generate(reader, writer)
    } yield ()

    def excludes(regex: String): SQLFilesGenerator = new SQLFilesGenerator(paths, reader.excludes(regex), writer)

    private def exec(script: String, xa: doobie.Transactor[IO]): IO[Int] =
      Update0(script, None).run.transact(xa).recoverWith { case NonFatal(e) => IO.raiseError(FailedScript(script, e)) }
  }

  /**
   * Reader Generator
   */

  def reader(reader: Reader) = new ReaderGeneratorBuilder(reader)

  class ReaderGeneratorBuilder(reader: Reader) {
    def writer(writer: Writer): ReaderGenerator = new ReaderGenerator(reader, writer)
  }

  class ReaderGenerator(reader: Reader, writer: Writer) {
    def generate(): IO[Unit] = Generator.generate(reader, writer)
  }

  private def generate(reader: Reader, writer: Writer): IO[Unit] = for {
    database <- reader.read()
    _ <- writer.write(database).toIO
  } yield ()
}
