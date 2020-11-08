package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ReaderSpec.Conf1

class ReaderSpec extends BaseSpec {
  private val (a0, a1, a2, a3) = (Reader.arg(0), Reader.arg(1), Reader.arg(2), Reader.arg(3))
  private val (a0o, a1o) = (Reader.argOpt(0), Reader.argOpt(1))

  describe("Reader") {
    it("should parse arguments") {
      a0.parse("gen") shouldBe Right("gen")
      a0.parse("") shouldBe Left(Errs.argNotFound(0))
      a1.parse("gen h2") shouldBe Right("h2")
      a1.parse("gen") shouldBe Left(Errs.argNotFound(1))

      a0o.parse("gen") shouldBe Right(Some("gen"))
      a0o.parse("") shouldBe Right(None)
      a1o.parse("gen h2") shouldBe Right(Some("h2"))
      a1o.parse("gen") shouldBe Right(None)
    }
    it("should parse flags") {
      Reader.flag("file").parse("--file test.txt") shouldBe Right("test.txt")
      Reader.flag("file").parse("--file a b") shouldBe Left(Errs.multipleFlagValues("file", List("a", "b")))
      Reader.flag("file").parse("--file") shouldBe Left(Errs.noFlagValue("file"))
      Reader.flag("file").parse("") shouldBe Left(Errs.flagNotFound("file"))

      Reader.flagOpt("file").parse("--file test.txt") shouldBe Right(Some("test.txt"))
      Reader.flagOpt("file").parse("--file a b") shouldBe Left(Errs.multipleFlagValues("file", List("a", "b")))
      Reader.flagOpt("file").parse("--file") shouldBe Left(Errs.noFlagValue("file"))
      Reader.flagOpt("file").parse("") shouldBe Right(None)

      Reader.flagList("file").parse("--file test.txt") shouldBe Right(List("test.txt"))
      Reader.flagList("file").parse("--file a b") shouldBe Right(List("a", "b"))
      Reader.flagList("file").parse("--file") shouldBe Right(List())
      Reader.flagList("file").parse("") shouldBe Left(Errs.flagNotFound("file"))

      Reader.flagNel("file").parse("--file test.txt") shouldBe Right(NonEmptyList.of("test.txt"))
      Reader.flagNel("file").parse("--file a b") shouldBe Right(NonEmptyList.of("a", "b"))
      Reader.flagNel("file").parse("--file") shouldBe Left(Errs.noFlagValue("file"))
      Reader.flagNel("file").parse("") shouldBe Left(Errs.flagNotFound("file"))

      Reader.flagBool("file").parse("--file test.txt") shouldBe Left(Errs.flagHasValue("file", "test.txt"))
      Reader.flagBool("file").parse("--file a b") shouldBe Left(Errs.flagHasValues("file", List("a", "b")))
      Reader.flagBool("file").parse("--file") shouldBe Right(true)
      Reader.flagBool("file").parse("") shouldBe Right(false)
    }
    it("should combine readers") {
      a0.and(a1).parse("gen h2") shouldBe Right("gen" -> "h2")
      a0.and(a1).parse("gen") shouldBe Left(Errs.argNotFound(1))
      a0.and(a1).parse("") shouldBe Left(Errs.argNotFound(0))
      a0.and(a1o).parse("gen h2") shouldBe Right("gen" -> Some("h2"))
      a0.and(a1o).parse("gen") shouldBe Right("gen" -> None)

      a0.and(a1).and(a2).parse("a b c") shouldBe Right((("a", "b"), "c"))
      a0.and(a1, a2).parse("a b c") shouldBe Right(("a", "b", "c"))

      a0.and(a1).and(a2).and(a3).parse("a b c d") shouldBe Right(((("a", "b"), "c"), "d"))
      a0.and(a1, a2, a3).parse("a b c d") shouldBe Right(("a", "b", "c", "d"))
    }
    it("should add alternative readers") {
      a0.or(a1).parse("gen h2") shouldBe Right("gen")
      a1.or(a0).parse("gen h2") shouldBe Right("h2")
      a0.or(a1).parse("gen") shouldBe Right("gen")
      a1.or(a0).parse("gen") shouldBe Right("gen")
      a0.or(a1).parse("") shouldBe Left(Errs.noAlternative(Errs.argNotFound(0), Errs.argNotFound(1)))
      a0.or(a1).or(a2).parse("") shouldBe Left(Errs.noAlternative(Errs.argNotFound(0), Errs.argNotFound(1), Errs.argNotFound(2)))
    }
    it("should validate reader result") {
      a0.parse("gen") shouldBe Right("gen")
      a0.validate(_ => true).parse("gen") shouldBe Right("gen")
      a0.validate(_ => false).parse("gen") shouldBe Left(Errs.validation("gen", None))
      a0.validate(_ => true, "is gen").parse("gen") shouldBe Right("gen")
      a0.inEnum("gen", "help").parse("gen") shouldBe Right("gen")
      a0.inEnum("gen", "help").parse("h2") shouldBe Left(Errs.badEnum("h2", Set("gen", "help")))
    }
    it("should not read twice an argument") {
      a0.and(a0).parse("gen h2") shouldBe Left(Errs.argRead(0))
    }
    it("should not read twice a flag") {
      Reader.flag("f").and(Reader.flag("f")).parse("--f h2") shouldBe Left(Errs.flagRead("f"))
    }
    it("should transform readers") {
      a0.map(_.length).parse("gen") shouldBe Right(3)
      a0.mapTry(s => Right(s.length)).parse("gen") shouldBe Right(3)
      a0.mapTry(s => Left(Errs.custom(s"err $s"))).parse("gen") shouldBe Left(Errs.custom("err gen"))
    }
    it("should parse object conf") {
      val reader: Reader[Conf1] = a0.and(a1o).map { case (a1, a2) => Conf1(a1, a2) }
      reader.parse("gen h2") shouldBe Right(Conf1("gen", Some("h2")))
      reader.parse("gen") shouldBe Right(Conf1("gen", None))
      reader.parse("") shouldBe Left(Errs.argNotFound(0))
    }
    it("should add readers based on read value") {
      val r = a0.on[String] {
        case "scala" => Reader.flag("conf")
        case "java" => Reader.flag("data")
        case v => Reader.error(s"Invalid value $v")
      }
      r.parse("scala --conf conf.json") shouldBe Right("scala" -> "conf.json")
      r.parse("java --data data.csv") shouldBe Right("java" -> "data.csv")
      r.parse("ruby") shouldBe Left(Errs.custom("Invalid value ruby"))
    }
  }
}

object ReaderSpec {

  case class Conf1(a1: String, a2: Option[String])

}
