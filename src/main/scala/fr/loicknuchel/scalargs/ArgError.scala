package fr.loicknuchel.scalargs

sealed trait ArgError {
  def getMessage: String
}

object ArgError {

  sealed abstract class BasicArgError(val message: String) extends ArgError {
    override def getMessage: String = message
  }

  final case class ArgumentNotFound(pos: Int,
                                    name: Option[String] = None,
                                    values: Option[Set[String]] = None) extends ArgError {
    override def getMessage: String = s"Missing argument ${pos + 1}${name.map(n => s" ($n${valuesMessage.map(m => s", $m").getOrElse("")})").getOrElse(valuesMessage.map(m => s" ($m)").getOrElse(""))}"

    private def valuesMessage: Option[String] = values.map(v => s"possible values: ${v.mkString(", ")}")
  }

  final case class ArgumentReadTwice(pos: Int, name: Option[String] = None) extends BasicArgError(s"Argument ${pos + 1}${name.map(n => s" ($n)").getOrElse("")} already read")

  final case class FlagNotFound(name: String) extends BasicArgError(s"Missing flag --$name")

  final case class FlagReadTwice(name: String) extends BasicArgError(s"Flag --$name already read")

  final case class NoFlagValue(name: String) extends BasicArgError(s"Flag --$name is present but has no value")

  final case class FlagHasValue(name: String, value: String) extends BasicArgError(s"Flag --$name has '$value' as value but expects no value")

  final case class EmptyFlagHasMultipleValues(name: String, values: List[String]) extends BasicArgError(s"Flag --$name has multiple values (${values.mkString(", ")}) but expects none")

  final case class UniqueFlagHasMultipleValues(name: String, values: List[String]) extends BasicArgError(s"Flag --$name has multiple values (${values.mkString(", ")}) but expects one")

  final case class ValidationError[A](value: A, validation: Option[String] = None) extends BasicArgError(validation.map(_ + s" (value: $value)").getOrElse(s"Value '$value' did not pass validation"))

  final case class InvalidEnumValue[A](value: A, values: Set[A]) extends BasicArgError(s"Value '$value' is not in the allowed values: ${values.mkString(", ")}")

  final case class NoValidAlternative(e1: ArgError, e2: ArgError, tail: List[ArgError] = List()) extends BasicArgError(s"Invalid arguments, here are your options (fix the error you want):${(e1 :: e2 :: tail).map("\n  - " + _.getMessage).mkString}")

  object NoValidAlternative {
    def apply(e1: ArgError, e2: ArgError, others: ArgError*): ArgError = NoValidAlternative(e1, e2, others.toList)
  }

  final case class CustomError(value: String) extends BasicArgError(value)

}
