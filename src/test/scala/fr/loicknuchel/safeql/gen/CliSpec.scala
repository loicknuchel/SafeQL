package fr.loicknuchel.safeql.gen

import java.io.ByteArrayOutputStream

import fr.loicknuchel.safeql.testingutils.BaseSpec

class CliSpec extends BaseSpec {
  describe("CLI") {
    it("should answer for help") {
      val out = new ByteArrayOutputStream()
      Console.withOut(out) {
        Cli.main("--help".split(" "))
      }
      out.toString shouldBe
        """Executing SafeQL CLI with args: --help
          |TODO: CLI help ^^
          |""".stripMargin
    }
    it("should print errors") {
      val out = new ByteArrayOutputStream()
      Console.withOut(out) {
        Cli.main("gen --flyway classpath:sql_migrations --output bad".split(" "))
      }
      out.toString shouldBe
        """Executing SafeQL CLI with args: gen --flyway classpath:sql_migrations --output bad
          |CLI Errors:
          |  - Invalid arguments, here are your options (fix the error you want):
          |    - Unknown output 'bad'
          |    - Missing flag --help
          |""".stripMargin
    }
  }
}
