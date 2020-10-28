package fr.loicknuchel.safeql.gen

import java.util.UUID

import cats.effect.IO
import fr.loicknuchel.safeql.gen.reader.{H2Reader, Reader}
import fr.loicknuchel.safeql.gen.writer.Writer
import fr.loicknuchel.safeql.utils.Extensions._
import org.flywaydb.core.Flyway
import org.flywaydb.core.internal.jdbc.DriverDataSource

object Generator {
  def generate(reader: Reader, writer: Writer): IO[Unit] = for {
    database <- reader.read()
    _ <- writer.write(database).toIO
  } yield ()

  def flyway(flywayLocations: String*): FlywayGeneratorBuilder = {
    val reader = H2Reader(
      url = s"jdbc:h2:mem:${UUID.randomUUID()};MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1",
      schema = Some("PUBLIC"),
      excludes = Some(".*flyway.*"))
    val flyway = Flyway.configure()
      .dataSource(new DriverDataSource(this.getClass.getClassLoader, reader.driver, reader.url, reader.user, reader.pass))
      .locations(flywayLocations: _*)
      .load()
    new FlywayGeneratorBuilder(reader, flyway)
  }

  class FlywayGeneratorBuilder(reader: H2Reader, flyway: Flyway) {
    def writer(writer: Writer): FlywayGenerator = new FlywayGenerator(reader, flyway, writer)

    def excludes(regex: String): FlywayGeneratorBuilder = new FlywayGeneratorBuilder(reader.excludes(regex), flyway)
  }

  class FlywayGenerator(reader: H2Reader, flyway: Flyway, writer: Writer) {
    def generate(): IO[Unit] = IO(flyway.migrate()).flatMap(_ => Generator.generate(reader, writer))

    def excludes(regex: String): FlywayGenerator = new FlywayGenerator(reader.excludes(regex), flyway, writer)
  }

  def reader(reader: Reader) = new ReaderGeneratorBuilder(reader)

  class ReaderGeneratorBuilder(reader: Reader) {
    def writer(writer: Writer): ReaderGenerator = new ReaderGenerator(reader, writer)
  }

  class ReaderGenerator(reader: Reader, writer: Writer) {
    def generate(): IO[Unit] = Generator.generate(reader, writer)
  }

}
