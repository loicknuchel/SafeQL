package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliConf.ReaderConf.{FlywayConf, JdbcConf, SqlFilesConf}
import fr.loicknuchel.safeql.gen.cli.CliConf.WriterConf.ScalaConf
import fr.loicknuchel.safeql.gen.writer.Writer
import fr.loicknuchel.safeql.gen.writer.Writer.IdentifierStrategy
import fr.loicknuchel.safeql.utils.Extensions._
import fr.loicknuchel.scalargs.{Errs, Reader}

private[gen] sealed trait CliConf extends Product with Serializable

private[gen] object CliConf {

  final case class HelpConf() extends CliConf

  final case class GenConf(reader: ReaderConf, writer: WriterConf) extends CliConf

  sealed trait ReaderConf extends Product with Serializable

  object ReaderConf {

    final case class FlywayConf(locations: NonEmptyList[String]) extends ReaderConf

    final case class SqlFilesConf(paths: List[String]) extends ReaderConf

    final case class JdbcConf(input: String, url: String) extends ReaderConf

  }

  sealed trait WriterConf extends Product with Serializable

  object WriterConf {

    final case class ScalaConf(directory: Option[String],
                               packageName: Option[String],
                               identifiers: Option[Writer.IdentifierStrategy],
                               configFile: Option[String]) extends WriterConf

  }

  private val flywayReader: Reader[FlywayConf] = Reader.flagNel("flyway").map(FlywayConf)
  private val sqlFilesReader: Reader[SqlFilesConf] = Reader.flagList("sql-files").map(SqlFilesConf)
  private val jdbcReader: Reader[JdbcConf] = Reader.flag("jdbc").inEnum("h2").and(Reader.flag("url")).map { case (in, url) => JdbcConf(in, url) }
  private val readerReader: Reader[ReaderConf] = flywayReader.or(sqlFilesReader).or(jdbcReader)

  private val scalaReader: Reader[ScalaConf] =
    Reader.flagOpt("dir").and(
      Reader.flagOpt("package"),
      Reader.flagOpt("identifiers").mapTry(_.map(s => IdentifierStrategy.byName(s).toRight(Errs.badEnum(s, IdentifierStrategy.all.map(_.toString)))).sequence),
      Reader.flagOpt("config")
    ).map { case (dir, pkg, idf, conf) => ScalaConf(dir, pkg, idf, conf) }
  private val writerReader: Reader[WriterConf] = Reader.flag("output").on {
    case "scala" => scalaReader
    case v => Reader.error(s"Unknown output '$v'")
  }.map { case (_, w) => w }

  private val genReader: Reader[GenConf] = Reader.arg(0).inEnum("gen").and(readerReader).and(writerReader).map { case ((_, r), w) => GenConf(r, w) }
  private val helpReader: Reader[HelpConf] = Reader.flagBool("help").validate(b => b, "Use --help to see doc").map(_ => HelpConf())
  val reader: Reader[CliConf] = genReader.or(helpReader)
}
