package fr.loicknuchel.safeql.gen.writer

import fr.loicknuchel.safeql.gen.Database
import fr.loicknuchel.safeql.gen.Database.Table
import fr.loicknuchel.safeql.utils.Extensions._
import fr.loicknuchel.safeql.utils.FileUtils

import scala.util.Try

trait Writer {
  def write(db: Database): Try[Unit] = writeFiles(generateFiles(db))

  def readFiles(): Try[Map[String, String]] = for {
    paths <- FileUtils.listFiles(rootFolderPath)
    files <- paths.map(p => FileUtils.read(p).map(c => (p, c))).sequence
  } yield files.toMap

  protected def writeFiles(files: Map[String, String]): Try[Unit] = for {
    _ <- FileUtils.delete(rootFolderPath)
    _ <- FileUtils.mkdirs(tablesFolderPath)
    _ <- files.map { case (path, content) => FileUtils.write(path, content) }.sequence
  } yield ()

  def generateFiles(db: Database): Map[String, String] = {
    val errors = getDatabaseErrors(db)
    if (errors.nonEmpty) throw new IllegalArgumentException(s"DatabaseConfig do not match with actual database, errors:${errors.map("\n - " + _).mkString}")
    val tables = db.schemas.flatMap(_.tables)
    val tableFiles = tables.map(t => tableFilePath(t) -> tableFile(t))
    ((listTablesFilePath, listTablesFile(tables)) :: tableFiles).toMap
  }

  protected def getDatabaseErrors(db: Database): List[String]

  protected[writer] def rootFolderPath: String

  protected[writer] def tablesFolderPath: String = rootFolderPath + "/tables"

  protected[writer] def listTablesFilePath: String = rootFolderPath + "/Tables.scala"

  protected[writer] def tableFilePath(t: Table): String

  protected def listTablesFile(tables: List[Table]): String

  protected def tableFile(table: Table): String
}

object Writer {

  trait IdentifierStrategy {
    def format(value: String): String
  }

  object IdentifierStrategy {

    class KeepNames extends IdentifierStrategy {
      // only avoid scala keywords
      override def format(value: String): String = value match {
        case "type" => "`type`"
        case v => v
      }
    }

    class UpperCase extends IdentifierStrategy {
      override def format(value: String): String = value.toUpperCase
    }

    val keepNames = new KeepNames
    val upperCase = new UpperCase
  }

}
