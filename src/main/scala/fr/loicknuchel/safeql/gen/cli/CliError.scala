package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliError.ParsingError
import fr.loicknuchel.scalargs.ArgError

private[gen] sealed trait CliError {
  def getMessage: String
}

private[gen] object CliError {

  sealed abstract class BasicCliError(val message: String) extends CliError {
    override def getMessage: String = message
  }

  final case class ParsingError(e: ArgError) extends BasicCliError(e.getMessage)

  final case class InvalidValue(kind: String, value: String) extends BasicCliError(s"'$value' is an invalid $kind")

  final case class MultiError(errs: NonEmptyList[String]) extends BasicCliError(s"Errors: ${errs.toList.mkString(", ")}")

  object MultiError {
    def apply(e1: String, others: String*): MultiError = new MultiError(NonEmptyList.of(e1, others: _*))
  }

  final case class UnsupportedOperation(op: String) extends BasicCliError(s"Unsupported operation: $op")

}

private[gen] case class CliErrors(head: CliError, tail: List[CliError]) {
  def getMessage: String = s"CLI Errors:${toList.map(e => "\n" + addIndentation("- " + e.getMessage)).mkString}"

  def nel: NonEmptyList[CliError] = NonEmptyList.of(head, tail: _*)

  def toList: List[CliError] = head :: tail

  private def addIndentation(text: String): String = text.split("\n").map("  " + _).mkString("\n")
}

private[gen] object CliErrors {
  def apply(e: CliError): CliErrors = new CliErrors(e, List())

  def from(e: ArgError): CliErrors = CliErrors(ParsingError(e), List())
}
