package fr.loicknuchel.safeql.utils

import fr.loicknuchel.safeql.testingutils.FileSpec

class FileUtilsSpec extends FileSpec {
  protected val root = "target/tests-FileUtilsSpec"

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
    it("should read a file with correct line breaks") {
      val noEmptyEndLine =
        """Bonjour,
          |ça va ?""".stripMargin
      FileUtils.write(s"$root/test.txt", noEmptyEndLine).get
      FileUtils.read(s"$root/test.txt").get shouldBe noEmptyEndLine

      val emptyEndLine =
        """Bonjour,
          |ça va ?
          |""".stripMargin
      FileUtils.write(s"$root/test.txt", emptyEndLine).get
      FileUtils.read(s"$root/test.txt").get shouldBe emptyEndLine

      val manyEmptyEndLines =
        """Bonjour,
          |
          |""".stripMargin
      FileUtils.write(s"$root/test.txt", manyEmptyEndLines).get
      FileUtils.read(s"$root/test.txt").get shouldBe manyEmptyEndLines
    }
    it("should list folder content") {
      FileUtils.mkdirs(s"$root/src/main/scala/fr/loicknuchel").get
      FileUtils.write(s"$root/src/main/scala/fr/loicknuchel/Main.scala", "public class Main").get
      FileUtils.write(s"$root/src/main/scala/README.md", "The readme").get

      FileUtils.getDirContent(s"$root/src/main/scala").get shouldBe Map(
        "README.md" -> "The readme",
        "fr/loicknuchel/Main.scala" -> "public class Main")
    }
  }
}
