package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.scalargs.ArgError._

sealed abstract class ArgError(val message: String)

object ArgError {

  final case class ArgumentNotFound(pos: Int) extends ArgError(s"Argument $pos does not exists!")

  final case class ArgumentAlreadyRead(pos: Int) extends ArgError(s"Argument $pos already read!")

  final case class FlagNotFound(name: String) extends ArgError(s"Flag '$name' does not exists!")

  final case class FlagAlreadyRead(name: String) extends ArgError(s"Flag '$name' already read!")

  final case class NoFlagValue(name: String) extends ArgError(s"Flag '$name' is present but has no value")

  final case class FlagHasValue(name: String, value: String) extends ArgError(s"Flag '$name' has '$value' as value but do not expect it")

  final case class MultipleFlagValues(name: String, values: List[String], expectOne: Boolean) extends ArgError(s"Flag '$name' has multiple values (${values.mkString(", ")}) but expects ${if (expectOne) "one" else "none"}")

  final case class NoValidAlternative(e1: ArgError, e2: ArgError, tail: List[ArgError]) extends ArgError(s"No valid alternative, fix one of: ${(e1 :: e2 :: tail).map("'" + _.message + "'").mkString(", ")}")

  final case class ValidationError[A](value: A, validation: Option[String]) extends ArgError(validation.map(_ + s" (value: $value)").getOrElse(s"Value '$value' did not pass validation"))

  final case class InvalidEnumValue[A](value: A, enum: Set[A]) extends ArgError(s"Value '$value' is not in the Enum set ${`enum`.mkString(", ")}")

  final case class CustomError(value: String) extends ArgError(value)

}

case class Errs(head: ArgError, tail: List[ArgError]) {
  def nel: NonEmptyList[ArgError] = NonEmptyList.of(head, tail: _*)

  def toList: List[ArgError] = head :: tail
}

object Errs {
  def apply(err: ArgError): Errs = new Errs(err, List())

  def argNotFound(pos: Int): Errs = Errs(ArgumentNotFound(pos))

  def argRead(pos: Int): Errs = Errs(ArgumentAlreadyRead(pos))

  def flagNotFound(name: String): Errs = Errs(FlagNotFound(name))

  def flagRead(name: String): Errs = Errs(FlagAlreadyRead(name))

  def noFlagValue(name: String): Errs = Errs(NoFlagValue(name))

  def flagHasValue(name: String, value: String): Errs = Errs(FlagHasValue(name, value))

  def flagHasValues(name: String, values: List[String]): Errs = Errs(MultipleFlagValues(name, values, expectOne = false))

  def multipleFlagValues(name: String, values: List[String]): Errs = Errs(MultipleFlagValues(name, values, expectOne = true))

  def noAlternative(e1: Errs, e2: Errs): Errs = (e1.head, e2.head) match {
    case (NoValidAlternative(a1, a2, ao), NoValidAlternative(b1, b2, bo)) => Errs(NoValidAlternative(a1, a2, (ao :+ b1 :+ b2) ++ bo))
    case (NoValidAlternative(a1, a2, ao), b) => Errs(NoValidAlternative(a1, a2, ao :+ b))
    case (a, NoValidAlternative(b1, b2, bo)) => Errs(NoValidAlternative(a, b1, b2 :: bo))
    case (a, b) => Errs(NoValidAlternative(a, b, List()))
  }

  def noAlternative(e1: Errs, e2: Errs, others: Errs*): Errs = others.foldLeft(noAlternative(e1, e2))((acc, e) => noAlternative(acc, e))

  def validation[A](value: A, validation: Option[String]): Errs = Errs(ValidationError(value, validation))

  def badEnum[A](value: A, enum: Set[A]): Errs = Errs(InvalidEnumValue(value, enum))

  def custom(value: String): Errs = Errs(CustomError(value))
}
