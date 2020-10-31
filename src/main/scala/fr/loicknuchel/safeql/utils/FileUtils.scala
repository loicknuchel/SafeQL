package fr.loicknuchel.safeql.utils

import java.io.File
import java.nio.file.{Files, Paths}

import fr.loicknuchel.safeql.utils.Extensions._

import scala.jdk.CollectionConverters._
import scala.util.Try

private[safeql] object FileUtils {
  def parent(path: String): String =
    path.split("/").dropRight(1).mkString("/")

  def listFiles(path: String, recursively: Boolean = true): Try[List[String]] = Try {
    def listDir(dir: File): List[File] = {
      if (dir.isDirectory) {
        Option(dir.listFiles).map(_.toList.flatMap(f => if (recursively) listDir(f) else List(f))).getOrElse(List())
      } else {
        List(dir)
      }
    }

    listDir(new File(path)).filter(_.isFile).map(_.getPath).sorted
  }

  // return a map for files with their relative path inside the directory and their content
  def getDirContent(path: String): Try[Map[String, String]] = {
    FileUtils.listFiles(path)
      .flatMap(_.map(p => FileUtils.read(p).map(c => (p.stripPrefix(path + "/"), c))).sequence)
      .map(_.toMap)
  }

  def read(path: String): Try[String] =
    Try(Files.readAllLines(Paths.get(path))).map(_.asScala.mkString("\n"))

  def write(path: String, content: String): Try[Unit] =
    mkdirs(parent(path)).flatMap(_ => Try(Files.write(Paths.get(path), content.getBytes)).map(_ => ()))

  def mkdirs(path: String): Try[Unit] =
    Try(Files.createDirectories(Paths.get(path))).map(_ => ())

  def delete(path: String): Try[Unit] = Try {
    def deleteDir(dir: File): Boolean = {
      if (dir.isDirectory) {
        Option(dir.listFiles).foreach(_.foreach(deleteDir))
      }
      dir.delete
    }

    deleteDir(new File(path))
    ()
  }
}
