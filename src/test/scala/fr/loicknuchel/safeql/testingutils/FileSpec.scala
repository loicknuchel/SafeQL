package fr.loicknuchel.safeql.testingutils

import fr.loicknuchel.safeql.utils.FileUtils
import org.scalatest.BeforeAndAfterEach

abstract class FileSpec extends BaseSpec with BeforeAndAfterEach {
  protected val root: String

  override protected def beforeEach(): Unit = FileUtils.mkdirs(root).get

  override protected def afterEach(): Unit = FileUtils.delete(root).get
}
