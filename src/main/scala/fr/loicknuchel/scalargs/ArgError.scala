package fr.loicknuchel.scalargs

import cats.data.NonEmptyList

sealed trait ArgError {
  val params: Params

  def getMessage: String
}

object ArgError {

  sealed abstract class BasicArgError(val message: String) extends ArgError {
    override def getMessage: String = message
  }

  final case class ArgumentNotFound(pos: Int,
                                    name: Option[String],
                                    values: Option[Set[String]],
                                    params: Params) extends ArgError {
    override def getMessage: String = s"Missing ${argName(pos, name)}${possibleValues(values)}"
  }

  object ArgumentNotFound {
    def apply(pos: Int, params: Params): ArgumentNotFound = new ArgumentNotFound(pos, None, None, params)

    def apply(pos: Int, name: String, params: Params): ArgumentNotFound = new ArgumentNotFound(pos, Some(name), None, params)

    def apply(pos: Int, values: Set[String], params: Params): ArgumentNotFound = new ArgumentNotFound(pos, None, Some(values), params)

    def apply(pos: Int, name: String, values: Set[String], params: Params): ArgumentNotFound = new ArgumentNotFound(pos, Some(name), Some(values), params)
  }

  final case class FlagNotFound(name: String, params: Params) extends BasicArgError(s"Missing ${flagName(name)}")

  final case class ArgumentReadTwice(pos: Int, name: Option[String], params: Params) extends BasicArgError(s"${argName(pos, name).capitalize} should not be used twice")

  object ArgumentReadTwice {
    def apply(pos: Int, params: Params): ArgumentReadTwice = new ArgumentReadTwice(pos, None, params)
  }

  final case class FlagReadTwice(name: String, params: Params) extends BasicArgError(s"${flagName(name)} should not be used twice")

  final case class NoFlagValue(name: String, params: Params) extends BasicArgError(s"Missing a value for ${flagName(name)}")

  final case class FlagHasValue(name: String, value: String, params: Params) extends BasicArgError(s"${flagName(name)} has '$value' as value but expects no value")

  final case class EmptyFlagHasMultipleValues(name: String, values: List[String], params: Params) extends BasicArgError(s"${flagName(name)} has multiple values (${values.mkString(", ")}) but expects none")

  final case class UniqueFlagHasMultipleValues(name: String, values: List[String], params: Params) extends BasicArgError(s"${flagName(name)} has multiple values (${values.mkString(", ")}) but expects only one")

  final case class ValidationError[A](value: A, validation: Option[String], params: Params) extends BasicArgError(validation.map(_ + s" (value: $value)").getOrElse(s"Value '$value' did not pass validation"))

  object ValidationError {
    def apply[A](value: A, params: Params): ValidationError[A] = new ValidationError(value, None, params)

    def apply[A](value: A, validation: String, params: Params): ValidationError[A] = new ValidationError(value, Some(validation), params)
  }

  final case class InvalidEnumValue[A](value: A, values: Set[A], params: Params) extends BasicArgError(s"Value '$value' is not allowed${possibleValues(Some(values))}")

  // TODO replace InvalidEnumValue once a Reader can differentiate arg & flag
  // final case class InvalidEnumValueForArgument[A](pos: Int, name: Option[String], value: A, values: Set[A], params: Params) extends BasicArgError(s"Value '$value' is not allowed for ${argName(pos, name)}${possibleValues(Some(values))}")
  // final case class InvalidEnumValueForFlag[A](name: String, value: A, values: Set[A], params: Params) extends BasicArgError(s"Value '$value' is not allowed for ${flagName(name)}${possibleValues(Some(values))}")

  final case class NoValidAlternative(e1: ArgError, e2: ArgError, tail: List[ArgError], params: Params) extends ArgError {
    override def getMessage: String = getRelevantErrors match {
      case NonEmptyList(e, List()) => e.getMessage
      case errs => s"Invalid arguments, here are your options (fix the error you want):${errs.map(e => "\n" + addIndentation("- " + e.getMessage)).toList.mkString}"
    }

    private def getRelevantErrors: NonEmptyList[ArgError] = {
      // keep only deepest errors (most relevant)
      val all = NonEmptyList.of(e1, e2) ++ tail
      val max = all.toList.map(_.params.size).max
      NonEmptyList.fromListUnsafe(all.filter(_.params.size == max)) // it's safe because max is always present somewhere
    }

    private def addIndentation(text: String): String = text.split("\n").map("  " + _).mkString("\n")
  }

  object NoValidAlternative {
    def apply(e1: ArgError, e2: ArgError, others: ArgError*): ArgError = NoValidAlternative(e1, e2, others.toList, (e1 :: e2 :: others.toList).map(_.params).maxBy(_.size))
  }

  final case class CustomError(value: String, params: Params) extends BasicArgError(value)

  private def argName(pos: Int, name: Option[String]): String = s"argument ${pos + 1}${name.map(n => s" ($n)").getOrElse("")}"

  private def flagName(name: String): String = s"--$name flag"

  private def possibleValues[A](values: Option[Set[A]]): String = values.map(v => s", possible values: ${v.mkString(", ")}").getOrElse("")
}
