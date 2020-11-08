package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.scalargs.ArgError._
import fr.loicknuchel.scalargs.ReaderSpec.Conf1
import org.scalatest.Assertion

class ReaderSpec extends BaseSpec {
  private val (a0, a1, a2, a3) = (Reader.arg(0), Reader.arg(1), Reader.arg(2), Reader.arg(3))
  private val (a0o, a1o) = (Reader.argOpt(0), Reader.argOpt(1))

  describe("Reader") {
    it("should read arguments") {
      on("gen   ") { p => a0.read(p) shouldBe Result("gen", p.arg(0)) }
      on("      ") { p => a0.read(p) shouldBe Result(ArgumentNotFound(0, p)) }
      on("gen h2") { p => a1.read(p) shouldBe Result("h2", p.arg(1)) }
      on("gen   ") { p => a1.read(p) shouldBe Result(ArgumentNotFound(1, p)) }

      on("gen   ") { p => a0o.read(p) shouldBe Result(Some("gen"), p.arg(0)) }
      on("      ") { p => a0o.read(p) shouldBe Result(None, p.arg(0)) }
      on("gen h2") { p => a1o.read(p) shouldBe Result(Some("h2"), p.arg(1)) }
      on("gen   ") { p => a1o.read(p) shouldBe Result(None, p.arg(1)) }
    }
    it("should read flags") {
      on("--file test.txt") { p => Reader.flag("file").read(p) shouldBe Result("test.txt", p.flag("file")) }
      on("--file a b     ") { p => Reader.flag("file").read(p) shouldBe Result(UniqueFlagHasMultipleValues("file", List("a", "b"), p)) }
      on("--file         ") { p => Reader.flag("file").read(p) shouldBe Result(NoFlagValue("file", p)) }
      on("               ") { p => Reader.flag("file").read(p) shouldBe Result(FlagNotFound("file", p)) }

      on("--file test.txt") { p => Reader.flagOpt("file").read(p) shouldBe Result(Some("test.txt"), p.flag("file")) }
      on("--file a b     ") { p => Reader.flagOpt("file").read(p) shouldBe Result(UniqueFlagHasMultipleValues("file", List("a", "b"), p)) }
      on("--file         ") { p => Reader.flagOpt("file").read(p) shouldBe Result(NoFlagValue("file", p)) }
      on("               ") { p => Reader.flagOpt("file").read(p) shouldBe Result(None, p.flag("file")) }

      on("--file test.txt") { p => Reader.flagList("file").read(p) shouldBe Result(List("test.txt"), p.flag("file")) }
      on("--file a b     ") { p => Reader.flagList("file").read(p) shouldBe Result(List("a", "b"), p.flag("file")) }
      on("--file         ") { p => Reader.flagList("file").read(p) shouldBe Result(List(), p.flag("file")) }
      on("               ") { p => Reader.flagList("file").read(p) shouldBe Result(FlagNotFound("file", p)) }

      on("--file test.txt") { p => Reader.flagNel("file").read(p) shouldBe Result(NonEmptyList.of("test.txt"), p.flag("file")) }
      on("--file a b     ") { p => Reader.flagNel("file").read(p) shouldBe Result(NonEmptyList.of("a", "b"), p.flag("file")) }
      on("--file         ") { p => Reader.flagNel("file").read(p) shouldBe Result(NoFlagValue("file", p)) }
      on("               ") { p => Reader.flagNel("file").read(p) shouldBe Result(FlagNotFound("file", p)) }

      on("--file test.txt") { p => Reader.flagBool("file").read(p) shouldBe Result(FlagHasValue("file", "test.txt", p)) }
      on("--file a b     ") { p => Reader.flagBool("file").read(p) shouldBe Result(EmptyFlagHasMultipleValues("file", List("a", "b"), p)) }
      on("--file         ") { p => Reader.flagBool("file").read(p) shouldBe Result(true, p.flag("file")) }
      on("               ") { p => Reader.flagBool("file").read(p) shouldBe Result(false, p.flag("file")) }

      on("--file test.txt") { p => Reader.hasFlag("file").read(p) shouldBe Result(FlagHasValue("file", "test.txt", p)) }
      on("--file a b     ") { p => Reader.hasFlag("file").read(p) shouldBe Result(EmptyFlagHasMultipleValues("file", List("a", "b"), p)) }
      on("--file         ") { p => Reader.hasFlag("file").read(p) shouldBe Result((), p.flag("file")) }
      on("               ") { p => Reader.hasFlag("file").read(p) shouldBe Result(FlagNotFound("file", p)) }
    }
    it("should combine readers") {
      on("gen h2") { p => a0.and(a1).read(p) shouldBe Result("gen" -> "h2", p.arg(0, 1)) }
      on("gen   ") { p => a0.and(a1).read(p) shouldBe Result(ArgumentNotFound(1, p.arg(0))) }
      on("      ") { p => a0.and(a1).read(p) shouldBe Result(ArgumentNotFound(0, p)) }
      on("gen h2") { p => a0.and(a1o).read(p) shouldBe Result("gen" -> Some("h2"), p.arg(0, 1)) }
      on("gen   ") { p => a0.and(a1o).read(p) shouldBe Result("gen" -> None, p.arg(0, 1)) }

      on("a b c") { p => a0.and(a1).and(a2).read(p) shouldBe Result((("a", "b"), "c"), p.arg(0, 1, 2)) }
      on("a b c") { p => a0.and(a1, a2).read(p) shouldBe Result(("a", "b", "c"), p.arg(0, 1, 2)) }

      on("a b c d") { p => a0.and(a1).and(a2).and(a3).read(p) shouldBe Result(((("a", "b"), "c"), "d"), p.arg(0, 1, 2, 3)) }
      on("a b c d") { p => a0.and(a1, a2, a3).read(p) shouldBe Result(("a", "b", "c", "d"), p.arg(0, 1, 2, 3)) }
    }
    it("should add alternative readers") {
      on("gen h2") { p => a0.or(a1).read(p) shouldBe Result("gen", p.arg(0)) }
      on("gen h2") { p => a1.or(a0).read(p) shouldBe Result("h2", p.arg(1)) }
      on("gen   ") { p => a0.or(a1).read(p) shouldBe Result("gen", p.arg(0)) }
      on("gen   ") { p => a1.or(a0).read(p) shouldBe Result("gen", p.arg(0)) }
      on("      ") { p => a0.or(a1).read(p) shouldBe Result(NoValidAlternative(ArgumentNotFound(0, p), ArgumentNotFound(1, p))) }
      on("      ") { p => a0.or(a1, a2).read(p) shouldBe Result(NoValidAlternative(ArgumentNotFound(0, p), ArgumentNotFound(1, p), ArgumentNotFound(2, p))) }
      on("      ") { p => a0.or(a1, a2, a3).read(p) shouldBe Result(NoValidAlternative(ArgumentNotFound(0, p), ArgumentNotFound(1, p), ArgumentNotFound(2, p), ArgumentNotFound(3, p))) }
      a0.or(a1).or(a2) shouldBe a0.or(a1, a2)
      a0.or(a1, a2).or(a3) shouldBe a0.or(a1, a2, a3)
      a0.or(a1).or(a2, a3) shouldBe a0.or(a1, a2, a3)
      a0.or(a1).or(a2).or(a3) shouldBe a0.or(a1, a2, a3)
    }
    it("should validate reader result") {
      on("gen") { p => a0.read(p) shouldBe Result("gen", p.arg(0)) }
      on("gen") { p => a0.validate(_ => true).read(p) shouldBe Result("gen", p.arg(0)) }
      on("gen") { p => a0.validate(_ => false).read(p) shouldBe Result(ValidationError("gen", p.arg(0))) }
      on("gen") { p => a0.validate(_ => true, "is gen").read(p) shouldBe Result("gen", p.arg(0)) }
      on("gen") { p => a0.validate(_ => false, "is gen").read(p) shouldBe Result(ValidationError("gen", "is gen", p.arg(0))) }
      on("gen") { p => a0.inEnum("gen", "help").read(p) shouldBe Result("gen", p.arg(0)) }
      on("h2 ") { p => a0.inEnum("gen", "help").read(p) shouldBe Result(InvalidEnumValue("h2", Set("gen", "help"), p.arg(0))) }
    }
    it("should not read twice an argument") {
      on("gen h2") { p => a0.and(a0).read(p) shouldBe Result(ArgumentReadTwice(0, p.arg(0))) }
    }
    it("should not read twice a flag") {
      on("--f h2") { p => Reader.flag("f").and(Reader.flag("f")).read(p) shouldBe Result(FlagReadTwice("f", p.flag("f"))) }
    }
    it("should transform readers") {
      on("gen") { p => a0.map(_.length).read(p) shouldBe Result(3, p.arg(0)) }
      on("gen") { p => a0.mapTry((s, _) => Right(s.length)).read(p) shouldBe Result(3, p.arg(0)) }
      on("gen") { p => a0.mapTry((s, p) => Left(CustomError(s"err $s", p))).read(p) shouldBe Result(CustomError("err gen", p.arg(0))) }
    }
    it("should read object conf") {
      val reader: Reader[Conf1] = a0.and(a1o).map { case (a1, a2) => Conf1(a1, a2) }
      on("gen h2") { p => reader.read(p) shouldBe Result(Conf1("gen", Some("h2")), p.arg(0, 1)) }
      on("gen   ") { p => reader.read(p) shouldBe Result(Conf1("gen", None), p.arg(0, 1)) }
      on("      ") { p => reader.read(p) shouldBe Result(ArgumentNotFound(0, p)) }
    }
    it("should add readers based on read value") {
      val r = a0.on[String] {
        case ("scala", _) => Reader.flag("conf")
        case ("java", _) => Reader.flag("data")
        case (v, p) => Reader.error(s"Invalid value $v", p)
      }
      on("scala --conf conf.json") { p => r.read(p) shouldBe Result("scala" -> "conf.json", p.arg(0).flag("conf")) }
      on("java --data data.csv  ") { p => r.read(p) shouldBe Result("java" -> "data.csv", p.arg(0).flag("data")) }
      on("ruby                  ") { p => r.read(p) shouldBe Result(CustomError("Invalid value ruby", p.arg(0))) }
    }
  }

  private def on(a: String)(f: Params => Assertion): Assertion = f(Params(a.trim))
}

object ReaderSpec {

  case class Conf1(a1: String, a2: Option[String])

}
