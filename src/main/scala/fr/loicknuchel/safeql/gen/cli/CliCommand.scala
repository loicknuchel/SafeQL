package fr.loicknuchel.safeql.gen.cli

import java.time.Instant

import cats.data.NonEmptyList
import cats.effect.IO
import fr.loicknuchel.safeql.gen.cli.CliConf.{ReaderConf, WriterConf}
import fr.loicknuchel.safeql.gen.cli.CliError.{InvalidValue, MultiError}
import fr.loicknuchel.safeql.gen.reader.H2Reader
import fr.loicknuchel.safeql.gen.writer.{ScalaWriter, Writer}
import fr.loicknuchel.safeql.gen.{Database, Generator}
import fr.loicknuchel.safeql.utils.Extensions._
import pureconfig.error.{ConfigReaderFailures, ConvertFailure, WrongSizeList}
import pureconfig.{ConfigCursor, ConfigReader, ConfigSource, Derivation}

private[gen] sealed trait CliCommand extends Product with Serializable {
  def out(text: => String): IO[Unit] = IO(println(text)) // cheap logger ^^

  def run: IO[Unit]
}

private[gen] object CliCommand {

  final case class GenWithFlyway(locations: List[String], writer: ScalaWriter) extends CliCommand {
    def run: IO[Unit] = Generator.flyway(locations: _*).writer(writer).generate()
  }

  final case class GenWithSqlFiles(paths: List[String], writer: ScalaWriter) extends CliCommand {
    def run: IO[Unit] = Generator.sqlFiles(paths).writer(writer).generate()
  }

  final case class GenWithH2Jdbc(url: String, writer: ScalaWriter) extends CliCommand {
    def run: IO[Unit] = Generator.reader(H2Reader(url, schema = Some("PUBLIC"))).writer(writer).generate()
  }

  final case class Help() extends CliCommand {
    override def run: IO[Unit] = out("TODO: CLI help ^^")
  }

  def from(now: Instant, conf: CliConf): Either[CliErrors, CliCommand] = conf match {
    case CliConf.GenConf(reader, writer) =>
      val w = writer match {
        case c: WriterConf.ScalaConf => buildScalaWriter(now, c)
      }
      reader match {
        case ReaderConf.FlywayConf(locations) => w.map(sw => GenWithFlyway(locations.toList, sw))
        case ReaderConf.SqlFilesConf(paths) => w.map(sw => GenWithSqlFiles(paths, sw))
        case ReaderConf.JdbcConf(_, url) => w.map(sw => GenWithH2Jdbc(url, sw))
      }
    case CliConf.HelpConf() => Right(Help())
  }

  private def buildScalaWriter(now: Instant, c: WriterConf.ScalaConf): Either[CliErrors, ScalaWriter] = {
    for {
      conf <- c.configFile.map(buildDbConf).getOrElse(Right(ScalaWriter.default.config))
      dir = c.directory.getOrElse(ScalaWriter.default.directory)
      pkg = c.packageName.getOrElse(ScalaWriter.default.packageName)
      idf = c.identifiers.getOrElse(ScalaWriter.default.identifierStrategy)
    } yield ScalaWriter(now, dir, pkg, idf, conf)
  }

  private[cli] def buildIdfStrategy(s: String): Either[CliErrors, Writer.IdentifierStrategy] =
    Writer.IdentifierStrategy.byName(s).toRight(CliErrors(InvalidValue("Identifier strategy", s)))

  private[cli] def buildDbConf(path: String): Either[CliErrors, ScalaWriter.DatabaseConfig] =
    ConfigSource.file(path).load[ScalaWriter.DatabaseConfig](DatabaseConfigReader.reader)
      .left.map(e => CliErrors(MultiError(NonEmptyList(e.head, e.tail.toList).map(_.description))))

  private[cli] object DatabaseConfigReader {

    import pureconfig.generic.semiauto._

    // I use `protected` instead of `private` to avoid scala compiler warnings, it thinks implicit vals are not used ^^
    protected implicit def nelReader[A: ConfigReader]: ConfigReader[NonEmptyList[A]] = (cur: ConfigCursor) => cur.asList
      .flatMap(_.map(implicitly[ConfigReader[A]].from).sequence)
      .flatMap(NonEmptyList.fromList(_).toEither(ConfigReaderFailures(ConvertFailure(WrongSizeList(1, 0), cur))))

    protected implicit val fieldReader: ConfigReader[ScalaWriter.FieldConfig] = deriveReader[ScalaWriter.FieldConfig]
    protected val tableSortFieldReaderFull: ConfigReader[ScalaWriter.TableConfig.Sort.Field] = deriveReader[ScalaWriter.TableConfig.Sort.Field]
    protected implicit val tableSortFieldReader: ConfigReader[ScalaWriter.TableConfig.Sort.Field] = (cur: ConfigCursor) =>
      cur.asString.map(s => ScalaWriter.TableConfig.Sort.Field(s)).fold(_ => tableSortFieldReaderFull.from(cur), Right(_))
    protected val tableSortReaderFull: ConfigReader[ScalaWriter.TableConfig.Sort] = deriveReader[ScalaWriter.TableConfig.Sort]
    protected implicit val tableSortReader: ConfigReader[ScalaWriter.TableConfig.Sort] = (cur: ConfigCursor) =>
      cur.asString.map(s => ScalaWriter.TableConfig.Sort(s)).fold(_ => tableSortReaderFull.from(cur), Right(_))
    protected implicit val tableReader: ConfigReader[ScalaWriter.TableConfig] = deriveReader[ScalaWriter.TableConfig]
    protected implicit val schemaReader: ConfigReader[ScalaWriter.SchemaConfig] = deriveReader[ScalaWriter.SchemaConfig]
    protected implicit val scaladocReader: ConfigReader[Option[Database.Table] => Option[String]] = (cur: ConfigCursor) => cur.asString.map(Some(_).filter(_.nonEmpty)).map(doc => _ => doc)
    protected implicit val databaseReader: ConfigReader[ScalaWriter.DatabaseConfig] = deriveReader[ScalaWriter.DatabaseConfig]

    val reader: Derivation[ConfigReader[ScalaWriter.DatabaseConfig]] = Derivation.Successful(databaseReader)
  }

}
