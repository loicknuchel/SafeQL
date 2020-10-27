package fr.loicknuchel.safeql.utils

import fr.loicknuchel.safeql.testingutils.BaseSpec
import org.scalatest.BeforeAndAfterEach

class FileUtilsSpec extends BaseSpec with BeforeAndAfterEach {
  private val root = "target/file-utils-tests"

  override protected def beforeEach(): Unit = FileUtils.mkdirs(root).get

  override protected def afterEach(): Unit = FileUtils.delete(root).get

  describe("FileUtils") {
    it("should compute parent path") {
      FileUtils.parent("usr/loic/test.txt") shouldBe "usr/loic"
      FileUtils.parent("usr/loic/test") shouldBe "usr/loic"
      FileUtils.parent("usr/loic/test/") shouldBe "usr/loic"
      FileUtils.parent("/usr/loic/test") shouldBe "/usr/loic"
      FileUtils.parent("/usr/loic/test/") shouldBe "/usr/loic"
    }
    it("should create folders and files, list them and delete them") {
      FileUtils.mkdirs(s"$root/src/main/scala/fr/lkn").get
      FileUtils.mkdirs(s"$root/src/test/scala/fr/lkn").get
      FileUtils.write(s"$root/readme.md", "Hello").get
      FileUtils.write(s"$root/LICENCE", "MIT").get
      FileUtils.write(s"$root/src/test/scala/fr/lkn/main.scala", "aaa").get
      FileUtils.listFiles(root).get shouldBe List(s"$root/LICENCE", s"$root/readme.md", s"$root/src/test/scala/fr/lkn/main.scala")
      FileUtils.listFiles(root, recursively = false).get shouldBe List(s"$root/LICENCE", s"$root/readme.md")

      FileUtils.read(s"$root/LICENCE").get shouldBe "MIT"
      FileUtils.write(s"$root/LICENCE", "APACHE").get
      FileUtils.read(s"$root/LICENCE").get shouldBe "APACHE"

      FileUtils.read(s"$root/src/test/scala/fr/lkn/main.scala").get shouldBe "aaa"
      FileUtils.delete(s"$root/src").get
      an[Exception] should be thrownBy FileUtils.read(s"$root/src/test/scala/fr/lkn/main.scala").get
    }
  }
}
