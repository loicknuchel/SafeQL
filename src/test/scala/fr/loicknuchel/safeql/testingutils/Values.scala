package fr.loicknuchel.safeql.testingutils

import java.util.UUID

import cats.effect.{ContextShift, IO}
import doobie.util.transactor.Transactor
import org.flywaydb.core.Flyway
import org.flywaydb.core.internal.jdbc.DriverDataSource

import scala.concurrent.ExecutionContext

object Values {
  def initSql(): (doobie.Transactor[IO], Flyway) = {
    val dbDriver = "org.h2.Driver"
    val dbUrl = s"jdbc:h2:mem:${UUID.randomUUID()};MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1"
    val dbUser = ""
    val dbPass = ""
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    val xa: doobie.Transactor[IO] = Transactor.fromDriverManager[IO](dbDriver, dbUrl, dbUser, dbPass)
    val flyway: Flyway = Flyway.configure()
      .dataSource(new DriverDataSource(this.getClass.getClassLoader, dbDriver, dbUrl, dbUser, dbPass))
      .locations("classpath:sql_migrations")
      .load()
    (xa, flyway)
  }

}
