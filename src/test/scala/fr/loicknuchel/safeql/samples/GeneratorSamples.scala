package fr.loicknuchel.safeql.samples

import java.time.Instant
import java.util.UUID

import fr.loicknuchel.safeql.gen.Generator
import fr.loicknuchel.safeql.gen.reader.H2Reader
import fr.loicknuchel.safeql.gen.writer.ScalaWriter

object GeneratorSamples {
  private val now = Instant.now()

  def main(args: Array[String]): Unit = generateFromFlyway()

  def generateFromFlyway(): Unit = {
    Generator
      .flyway("classpath:sql_migrations")
      .writer(ScalaWriter(now, packageName = "com.company.db"))
      .generate().unsafeRunSync()
  }

  def generateFromSQLFiles(): Unit = {
    Generator
      .sqlFiles(List("src/test/resources/sql_migrations/V1__test_schema.sql"))
      .writer(ScalaWriter(now, packageName = "com.company.db"))
      .generate().unsafeRunSync()
  }

  def generateFromDatabase(): Unit = {
    Generator
      .reader(H2Reader(s"jdbc:h2:mem:${UUID.randomUUID()};MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1"))
      .writer(ScalaWriter(now, packageName = "com.company.db"))
      .generate().unsafeRunSync()
  }
}
