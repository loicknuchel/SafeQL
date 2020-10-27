package fr.loicknuchel.safeql.utils

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import doobie.syntax.string._
import doobie.util.fragment.Fragment
import doobie.util.fragment.Fragment.const0

import scala.collection.{BuildFrom, mutable}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object Extensions {

  implicit class RichOption[A](val in: Option[A]) extends AnyVal {
    def toEither[E](e: => E): Either[E, A] = in match {
      case Some(v) => Right(v)
      case None => Left(e)
    }
  }

  implicit class RichTry[A](val in: Try[A]) extends AnyVal {
    def toIO: IO[A] = in match {
      case Success(a) => IO.pure(a)
      case Failure(e) => IO.raiseError(e)
    }
  }

  implicit class RichEither[E, A](val in: Either[E, A]) extends AnyVal {
    def asTry(f: E => Throwable): Try[A] = in match {
      case Right(a) => Success(a)
      case Left(e) => Failure(f(e))
    }
  }

  implicit class RichIO[A](val in: IO[A]) extends AnyVal {
    def recoverWith[B](pf: PartialFunction[Throwable, IO[A]]): IO[A] = implicitly[MonadError[IO, Throwable]].recoverWith(in)(pf)
  }

  implicit class RichIterableOnce[A, M[X] <: IterableOnce[X]](val in: M[A]) extends AnyVal {
    def toNel: Either[IllegalArgumentException, NonEmptyList[A]] = {
      val list = in.iterator.to(List)
      NonEmptyList.fromList(list).toEither(new IllegalArgumentException("List should not be empty"))
    }

    def mk(concat: (A, A) => A): Option[A] = in.iterator.to(List) match {
      case Nil => None
      case head :: Nil => Some(head)
      case head :: tail => Some(tail.foldLeft(head)(concat))
    }
  }

  implicit class RichIterableOnceTry[A, M[X] <: IterableOnce[X]](val in: M[Try[A]]) extends AnyVal {
    def sequence(implicit cbf: BuildFrom[M[Try[A]], A, M[A]]): Try[M[A]] = {
      val init = Try(cbf.newBuilder(in) -> List.empty[Throwable])
      in.iterator.foldLeft(init) { (acc, cur) =>
        acc.flatMap { case (results, errors) =>
          cur.map { result => (results += result, errors) }
            .recover { case NonFatal(error) => (results, error +: errors) }
        }
      }.flatMap(sequenceResult[A, M])
    }
  }

  implicit class RichIterableOnceFragment[M[X] <: IterableOnce[X]](val in: M[Fragment]) extends AnyVal {
    def mkFragment(sep: Fragment): Fragment = in.iterator.to(List) match {
      case Nil => fr0""
      case head :: Nil => head
      case head :: tail => tail.foldLeft(head)(_ ++ sep ++ _)
    }

    def mkFragment(sep: String): Fragment = mkFragment(const0(sep))
  }

  implicit class RichNonEmptyListFragment(val in: NonEmptyList[Fragment]) extends AnyVal {
    def mkFragment(sep: Fragment): Fragment = in.tail.foldLeft(in.head)(_ ++ sep ++ _)

    def mkFragment(sep: String): Fragment = mkFragment(const0(sep))
  }

  private def sequenceResult[A, M[X] <: IterableOnce[X]](in: (mutable.Builder[A, M[A]], List[Throwable])): Try[M[A]] = {
    sequenceResultEither(in).left.map(_.flatMap {
      case MultiException(errs) => errs
      case e => NonEmptyList.of(e)
    }).asTry(errs => if (errs.length == 1) errs.head else MultiException(errs))
  }

  private def sequenceResultEither[E, A, M[X] <: IterableOnce[X]](in: (mutable.Builder[A, M[A]], List[E])): Either[NonEmptyList[E], M[A]] = {
    val (results, errors) = in
    errors.reverse.toNel.swap.map(_ => results.result())
  }
}
