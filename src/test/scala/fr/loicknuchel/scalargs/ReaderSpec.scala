package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError._
import fr.loicknuchel.scalargs.ReaderSpec.Conf1

class ReaderSpec extends BaseSpec {
  private val (a0, a1, a2, a3) = (Reader.arg(0), Reader.arg(1), Reader.arg(2), Reader.arg(3))
  private val (a0o, a1o) = (Reader.argOpt(0), Reader.argOpt(1))

  describe("Reader") {
    it("should read arguments") {
      a0.read("gen").get shouldBe Some("gen")
      a0.read("").err shouldBe Some(ArgumentNotFound(0))
      a1.read("gen h2").get shouldBe Some("h2")
      a1.read("gen").err shouldBe Some(ArgumentNotFound(1))

      a0o.read("gen").get shouldBe Some(Some("gen"))
      a0o.read("").get shouldBe Some(None)
      a1o.read("gen h2").get shouldBe Some(Some("h2"))
      a1o.read("gen").get shouldBe Some(None)
    }
    it("should read flags") {
      Reader.flag("file").read("--file test.txt").get shouldBe Some("test.txt")
      Reader.flag("file").read("--file a b").err shouldBe Some(UniqueFlagHasMultipleValues("file", List("a", "b")))
      Reader.flag("file").read("--file").err shouldBe Some(NoFlagValue("file"))
      Reader.flag("file").read("").err shouldBe Some(FlagNotFound("file"))

      Reader.flagOpt("file").read("--file test.txt").get shouldBe Some(Some("test.txt"))
      Reader.flagOpt("file").read("--file a b").err shouldBe Some(UniqueFlagHasMultipleValues("file", List("a", "b")))
      Reader.flagOpt("file").read("--file").err shouldBe Some(NoFlagValue("file"))
      Reader.flagOpt("file").read("").get shouldBe Some(None)

      Reader.flagList("file").read("--file test.txt").get shouldBe Some(List("test.txt"))
      Reader.flagList("file").read("--file a b").get shouldBe Some(List("a", "b"))
      Reader.flagList("file").read("--file").get shouldBe Some(List())
      Reader.flagList("file").read("").err shouldBe Some(FlagNotFound("file"))

      Reader.flagNel("file").read("--file test.txt").get shouldBe Some(NonEmptyList.of("test.txt"))
      Reader.flagNel("file").read("--file a b").get shouldBe Some(NonEmptyList.of("a", "b"))
      Reader.flagNel("file").read("--file").err shouldBe Some(NoFlagValue("file"))
      Reader.flagNel("file").read("").err shouldBe Some(FlagNotFound("file"))

      Reader.flagBool("file").read("--file test.txt").err shouldBe Some(FlagHasValue("file", "test.txt"))
      Reader.flagBool("file").read("--file a b").err shouldBe Some(EmptyFlagHasMultipleValues("file", List("a", "b")))
      Reader.flagBool("file").read("--file").get shouldBe Some(true)
      Reader.flagBool("file").read("").get shouldBe Some(false)

      Reader.hasFlag("file").read("--file test.txt").err shouldBe Some(FlagHasValue("file", "test.txt"))
      Reader.hasFlag("file").read("--file a b").err shouldBe Some(EmptyFlagHasMultipleValues("file", List("a", "b")))
      Reader.hasFlag("file").read("--file").get shouldBe Some(())
      Reader.hasFlag("file").read("").err shouldBe Some(FlagNotFound("file"))
    }
    it("should combine readers") {
      a0.and(a1).read("gen h2").get shouldBe Some("gen" -> "h2")
      a0.and(a1).read("gen").err shouldBe Some(ArgumentNotFound(1))
      a0.and(a1).read("").err shouldBe Some(ArgumentNotFound(0))
      a0.and(a1o).read("gen h2").get shouldBe Some("gen" -> Some("h2"))
      a0.and(a1o).read("gen").get shouldBe Some("gen" -> None)

      a0.and(a1).and(a2).read("a b c").get shouldBe Some((("a", "b"), "c"))
      a0.and(a1, a2).read("a b c").get shouldBe Some(("a", "b", "c"))

      a0.and(a1).and(a2).and(a3).read("a b c d").get shouldBe Some(((("a", "b"), "c"), "d"))
      a0.and(a1, a2, a3).read("a b c d").get shouldBe Some(("a", "b", "c", "d"))
    }
    it("should add alternative readers") {
      a0.or(a1).read("gen h2").get shouldBe Some("gen")
      a1.or(a0).read("gen h2").get shouldBe Some("h2")
      a0.or(a1).read("gen").get shouldBe Some("gen")
      a1.or(a0).read("gen").get shouldBe Some("gen")
      a0.or(a1).read("").err shouldBe Some(NoValidAlternative(ArgumentNotFound(0), ArgumentNotFound(1)))
      a0.or(a1, a2).read("").err shouldBe Some(NoValidAlternative(ArgumentNotFound(0), ArgumentNotFound(1), ArgumentNotFound(2)))
      a0.or(a1).or(a2) shouldBe a0.or(a1, a2)
      a0.or(a1, a2, a3).read("").err shouldBe Some(NoValidAlternative(ArgumentNotFound(0), ArgumentNotFound(1), ArgumentNotFound(2), ArgumentNotFound(3)))
      a0.or(a1, a2).or(a3) shouldBe a0.or(a1, a2, a3)
      a0.or(a1).or(a2, a3) shouldBe a0.or(a1, a2, a3)
      a0.or(a1).or(a2).or(a3) shouldBe a0.or(a1, a2, a3)
    }
    it("should validate reader result") {
      a0.read("gen").get shouldBe Some("gen")
      a0.validate(_ => true).read("gen").get shouldBe Some("gen")
      a0.validate(_ => false).read("gen").err shouldBe Some(ValidationError("gen"))
      a0.validate(_ => true, "is gen").read("gen").get shouldBe Some("gen")
      a0.inEnum("gen", "help").read("gen").get shouldBe Some("gen")
      a0.inEnum("gen", "help").read("h2").err shouldBe Some(InvalidEnumValue("h2", Set("gen", "help")))
    }
    it("should not read twice an argument") {
      a0.and(a0).read("gen h2").err shouldBe Some(ArgumentReadTwice(0))
    }
    it("should not read twice a flag") {
      Reader.flag("f").and(Reader.flag("f")).read("--f h2").err shouldBe Some(FlagReadTwice("f"))
    }
    it("should transform readers") {
      a0.map(_.length).read("gen").get shouldBe Some(3)
      a0.mapTry(s => Right(s.length)).read("gen").get shouldBe Some(3)
      a0.mapTry(s => Left(CustomError(s"err $s"))).read("gen").err shouldBe Some(CustomError("err gen"))
    }
    it("should read object conf") {
      val reader: Reader[Conf1] = a0.and(a1o).map { case (a1, a2) => Conf1(a1, a2) }
      reader.read("gen h2").get shouldBe Some(Conf1("gen", Some("h2")))
      reader.read("gen").get shouldBe Some(Conf1("gen", None))
      reader.read("").err shouldBe Some(ArgumentNotFound(0))
    }
    it("should add readers based on read value") {
      val r = a0.on[String] {
        case "scala" => Reader.flag("conf")
        case "java" => Reader.flag("data")
        case v => Reader.error(s"Invalid value $v")
      }
      r.read("scala --conf conf.json").get shouldBe Some("scala" -> "conf.json")
      r.read("java --data data.csv").get shouldBe Some("java" -> "data.csv")
      r.read("ruby").err shouldBe Some(CustomError("Invalid value ruby"))
    }
  }
}

object ReaderSpec {

  case class Conf1(a1: String, a2: Option[String])

}
