package fr.loicknuchel.safeql.gen

import java.time.Instant

import fr.loicknuchel.safeql.gen.cli.{CliCommand, CliConf, CliErrors}

/**
 * launch it using sbt: `sbt "run ...args..."`
 * or using sbt-pack: `target/pack/bin/safeql ...args...` (after having run `sbt pack`)
 */
// FIXME: add optional `exclude` param
// FIXME: add optional `schema` param
// FIXME: add `--help` command
// FIXME: add reporting to commands (generated files in...)
object Cli {
  // allows to generate tables from CLI with command like:
  // `safeql gen --flyway classpath:sql_migrations --output scala --dir src/test/scala --package fr.loicknuchel.safeql.testingutils.database --config dbconf.json`
  // `safeql gen --sql-files V1__test_schema.sql --output scala --dir src/test/scala --package fr.loicknuchel.safeql.testingutils.database --config dbconf.json`
  // `safeql gen --input h2 --url jdbc:h2:mem --schema PUBLIC --output scala --dir src/test/scala --package fr.loicknuchel.safeql.testingutils.database --config dbconf.json`
  def main(args: Array[String]): Unit = {
    println(s"Executing SafeQL CLI with args: ${args.mkString(" ")}")
    val now = Instant.now()
    CliConf.reader.read(args).toEither.left.map(CliErrors.from).flatMap(CliCommand.from(now, _)) match {
      case Left(errs) => println(errs.getMessage)
      case Right(cmd) => cmd.run.unsafeRunSync()
    }
  }
}
