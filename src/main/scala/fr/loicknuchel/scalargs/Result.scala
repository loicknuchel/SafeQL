package fr.loicknuchel.scalargs

sealed trait Result[+A] {
  val params: Params

  def map[B](f: A => B): Result[B]

  def flatMap[B](f: A => Result[B]): Result[B]

  def filter(p: A => Boolean, e: A => Errs): Result[A]

  def filter(p: A => Boolean, validation: Option[String]): Result[A] = filter(p, a => Errs.validation(a, validation))

  def filter(p: A => Boolean, validation: String): Result[A] = filter(p, Some(validation))

  def filter(p: A => Boolean): Result[A] = filter(p, None)

  // like `flatMap` but with Params as parameter and keep the previous result
  def chain[B](f: (A, Params) => Result[B]): Result[(A, B)] = flatMap(a => f(a, params).map(b => (a, b)))

  def orElse[B >: A](r: => Result[B]): Result[B]

  def toEither: Either[Errs, (A, Params)]
}

object Result {

  case class Success[A](value: A, params: Params) extends Result[A] {
    override def map[B](f: A => B): Result[B] = Success(f(value), params)

    override def flatMap[B](f: A => Result[B]): Result[B] = f(value)

    override def filter(p: A => Boolean, e: A => Errs): Result[A] = if (p(value)) this else Failure(e(value), params)

    override def orElse[B >: A](r: => Result[B]): Result[B] = this

    override def toEither: Either[Errs, (A, Params)] = Right(value -> params)
  }

  case class Failure(errs: Errs, params: Params) extends Result[Nothing] {
    override def map[B](f: Nothing => B): Result[B] = this

    override def flatMap[B](f: Nothing => Result[B]): Result[B] = this

    override def filter(p: Nothing => Boolean, e: Nothing => Errs): Result[Nothing] = this

    override def orElse[B >: Nothing](r: => Result[B]): Result[B] = r match {
      case res: Success[_] => res
      case Result.Failure(e, _) => Result.Failure(Errs.noAlternative(errs, e), params)
    }

    override def toEither: Either[Errs, (Nothing, Params)] = Left(errs)
  }

}
