package fr.loicknuchel.safeql.testingutils

import org.scalatest.BeforeAndAfterEach

abstract class SqlSpec extends BaseSpec with BeforeAndAfterEach {
  protected val (xa, flyway) = Values.initSql()

  override def beforeEach(): Unit = {
    flyway.migrate()
    ()
  }

  override def afterEach(): Unit = {
    flyway.clean()
    ()
  }
}
