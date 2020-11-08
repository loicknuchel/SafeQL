package fr.loicknuchel.scalargs

import fr.loicknuchel.scalargs.ArgError.{NoValidAlternative, ValidationError}

sealed trait Result[+A] extends Product with Serializable {
  val params: Params

  def map[B](f: A => B): Result[B]

  def flatMap[B](f: (A, Params) => Result[B]): Result[B]

  // like `flatMap` but keep the previous result
  def chain[B](f: (A, Params) => Result[B]): Result[(A, B)] = flatMap((a, p) => f(a, p).map(b => (a, b)))

  def filter(p: A => Boolean, e: A => ArgError): Result[A]

  def filter(p: A => Boolean, validation: Option[String]): Result[A] = filter(p, a => ValidationError(a, validation))

  def filter(p: A => Boolean, validation: String): Result[A] = filter(p, Some(validation))

  def filter(p: A => Boolean): Result[A] = filter(p, None)

  def orElse[B >: A](r: => Result[B]): Result[B]

  def orElse[B >: A](r1: => Result[B], r2: => Result[B]): Result[B]

  def orElse[B >: A](r1: => Result[B], r2: => Result[B], r3: => Result[B]): Result[B]

  def get: Option[A]

  def err: Option[ArgError]

  def toEither: Either[ArgError, A]
}

object Result {

  def apply[A](value: A, params: Params): Result[A] = Success(value, params)

  def fail[A](e: ArgError, params: Params): Result[A] = Failure(e, params)

  def from[A](e: Either[ArgError, A], params: Params): Result[A] = e.fold(Result.Failure(_, params), Result.Success(_, params))

  case class Success[A](value: A, params: Params) extends Result[A] {
    override def map[B](f: A => B): Result[B] = Success(f(value), params)

    override def flatMap[B](f: (A, Params) => Result[B]): Result[B] = f(value, params)

    override def filter(p: A => Boolean, e: A => ArgError): Result[A] = if (p(value)) this else Failure(e(value), params)

    override def orElse[B >: A](r: => Result[B]): Result[B] = this

    override def orElse[B >: A](r1: => Result[B], r2: => Result[B]): Result[B] = this

    override def orElse[B >: A](r1: => Result[B], r2: => Result[B], r3: => Result[B]): Result[B] = this

    override def get: Option[A] = Some(value)

    override def err: Option[ArgError] = None

    override def toEither: Either[ArgError, A] = Right(value)
  }

  case class Failure(error: ArgError, params: Params) extends Result[Nothing] {
    override def map[B](f: Nothing => B): Result[B] = this

    override def flatMap[B](f: (Nothing, Params) => Result[B]): Result[B] = this

    override def filter(p: Nothing => Boolean, e: Nothing => ArgError): Result[Nothing] = this

    override def orElse[B >: Nothing](r: => Result[B]): Result[B] = r match {
      case res: Success[_] => res
      case Result.Failure(e, _) => Result.Failure(NoValidAlternative(error, e), params)
    }

    override def orElse[B >: Nothing](r1: => Result[B], r2: => Result[B]): Result[B] = r1 match {
      case res: Success[_] => res
      case Result.Failure(e1, _) => r2 match {
        case res: Success[_] => res
        case Result.Failure(e2, _) => Result.Failure(NoValidAlternative(error, e1, List(e2)), params)
      }
    }

    override def orElse[B >: Nothing](r1: => Result[B], r2: => Result[B], r3: => Result[B]): Result[B] = r1 match {
      case res: Success[_] => res
      case Result.Failure(e1, _) => r2 match {
        case res: Success[_] => res
        case Result.Failure(e2, _) => r3 match {
          case res: Success[_] => res
          case Result.Failure(e3, _) => Result.Failure(NoValidAlternative(error, e1, List(e2, e3)), params)
        }
      }
    }

    override def get: Option[Nothing] = None

    override def err: Option[ArgError] = Some(error)

    override def toEither: Either[ArgError, Nothing] = Left(error)
  }

}
