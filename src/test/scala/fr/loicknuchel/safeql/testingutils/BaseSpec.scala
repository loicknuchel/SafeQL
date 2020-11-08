package fr.loicknuchel.safeql.testingutils

import java.time.Instant

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

abstract class BaseSpec extends AnyFunSpec with Matchers {
  // should be identical to generated db (see `fr.loicknuchel.safeql.testingutils.database.Tables`)
  protected val now: Instant = Instant.parse("2020-11-08T08:23:47.530962038Z")
}
