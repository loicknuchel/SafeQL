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
          |Errors:
          | - No valid alternative, fix one of: 'Unknown output 'bad'', 'Use --help to see doc (value: false)'
          |""".stripMargin
    }
  }
}
