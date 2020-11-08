package fr.loicknuchel.scalargs

import fr.loicknuchel.scalargs.ArgError.{NoValidAlternative, ValidationError}

sealed trait Result[+A] extends Product with Serializable {
  val params: Params

  def map[B](f: A => B): Result[B]

  def flatMap[B](f: (A, Params) => Result[B]): Result[B]

  def and[B](f: Params => Result[B]): Result[(A, B)] = flatMap((a, p) => f(p).map(b => (a, b)))

  def chain[B](f: (A, Params) => Result[B]): Result[(A, B)] = flatMap((a, p) => f(a, p).map(b => (a, b)))

  def filter(p: A => Boolean, e: (A, Params) => ArgError): Result[A]

  def filter(p: A => Boolean, validation: Option[String]): Result[A] = filter(p, ValidationError(_, validation, _))

  def filter(p: A => Boolean, validation: String): Result[A] = filter(p, Some(validation))

  def filter(p: A => Boolean): Result[A] = filter(p, None)

  def orElse[B >: A](r: => Result[B]): Result[B]

  def orElse[B >: A](r1: => Result[B], r2: => Result[B]): Result[B]

  def orElse[B >: A](r1: => Result[B], r2: => Result[B], r3: => Result[B]): Result[B]

  def toEither: Either[ArgError, A]
}

object Result {

  def apply[A](value: A, params: Params): Success[A] = Success(value, params)

  def apply(error: ArgError): Failure = Failure(error)

  case class Success[A](value: A, params: Params) extends Result[A] {
    override def map[B](f: A => B): Result[B] = Success(f(value), params)

    override def flatMap[B](f: (A, Params) => Result[B]): Result[B] = f(value, params)

    override def filter(p: A => Boolean, e: (A, Params) => ArgError): Result[A] = if (p(value)) this else Failure(e(value, params))

    override def orElse[B >: A](r: => Result[B]): Result[B] = this

    override def orElse[B >: A](r1: => Result[B], r2: => Result[B]): Result[B] = this

    override def orElse[B >: A](r1: => Result[B], r2: => Result[B], r3: => Result[B]): Result[B] = this

    override def toEither: Either[ArgError, A] = Right(value)
  }

  case class Failure(error: ArgError) extends Result[Nothing] {
    val params: Params = error.params

    override def map[B](f: Nothing => B): Result[B] = this

    override def flatMap[B](f: (Nothing, Params) => Result[B]): Result[B] = this

    override def filter(p: Nothing => Boolean, e: (Nothing, Params) => ArgError): Result[Nothing] = this

    override def orElse[B >: Nothing](r: => Result[B]): Result[B] = r match {
      case res: Success[_] => res
      case Failure(e) => Result(NoValidAlternative(error, e))
    }

    override def orElse[B >: Nothing](r1: => Result[B], r2: => Result[B]): Result[B] = r1 match {
      case res: Success[_] => res
      case Failure(e1) => r2 match {
        case res: Success[_] => res
        case Failure(e2) => Result(NoValidAlternative(error, e1, e2))
      }
    }

    override def orElse[B >: Nothing](r1: => Result[B], r2: => Result[B], r3: => Result[B]): Result[B] = r1 match {
      case res: Success[_] => res
      case Failure(e1) => r2 match {
        case res: Success[_] => res
        case Failure(e2) => r3 match {
          case res: Success[_] => res
          case Failure(e3) => Result(NoValidAlternative(error, e1, e2, e3))
        }
      }
    }

    override def toEither: Either[ArgError, Nothing] = Left(error)
  }

}
