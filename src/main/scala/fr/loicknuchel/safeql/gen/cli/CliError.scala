package fr.loicknuchel.safeql.gen.cli

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.gen.cli.CliError.ParsingError
import fr.loicknuchel.scalargs.{ArgError, Errs}

private[gen] sealed abstract class CliError(val message: String)

private[gen] object CliError {

  final case class ParsingError(e: ArgError) extends CliError(e.message)

  final case class InvalidValue(kind: String, value: String) extends CliError(s"'$value' is an invalid $kind")

  final case class MultiError(errs: NonEmptyList[String]) extends CliError(s"Errors: ${errs.toList.mkString(", ")}")

  object MultiError {
    def apply(e1: String, others: String*): MultiError = new MultiError(NonEmptyList.of(e1, others: _*))
  }

  final case class UnsupportedOperation(op: String) extends CliError(s"Unsupported operation: $op")

}

private[gen] case class CliErrors(head: CliError, tail: List[CliError]) {
  def message: String = s"Errors:${toList.map("\n - " + _.message).mkString}"

  def nel: NonEmptyList[CliError] = NonEmptyList.of(head, tail: _*)

  def toList: List[CliError] = head :: tail
}

private[gen] object CliErrors {
  def apply(e: CliError): CliErrors = new CliErrors(e, List())

  def from(e: Errs): CliErrors = CliErrors(ParsingError(e.head), e.tail.map(ParsingError))
}
