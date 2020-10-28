package fr.loicknuchel.safeql.testingutils

import java.util.UUID

import cats.effect.{ContextShift, IO}
import doobie.util.transactor.Transactor
import org.flywaydb.core.Flyway
import org.flywaydb.core.internal.jdbc.DriverDataSource
import org.scalatest.BeforeAndAfterEach

import scala.concurrent.ExecutionContext

abstract class SqlSpec extends BaseSpec with BeforeAndAfterEach {
  protected val dbDriver = "org.h2.Driver"
  protected val dbUrl = s"jdbc:h2:mem:${UUID.randomUUID()};MODE=PostgreSQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1"
  protected val dbUser = ""
  protected val dbPass = ""
  private implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  protected val xa: doobie.Transactor[IO] = Transactor.fromDriverManager[IO](dbDriver, dbUrl, dbUser, dbPass)
  private val flyway: Flyway = Flyway.configure()
    .dataSource(new DriverDataSource(this.getClass.getClassLoader, dbDriver, dbUrl, dbUser, dbPass))
    .locations("classpath:sql_migrations")
    .load()

  override def beforeEach(): Unit = {
    flyway.migrate()
    ()
  }

  override def afterEach(): Unit = {
    flyway.clean()
    ()
  }
}
