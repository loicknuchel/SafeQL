package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.scalargs.Reader._

sealed trait Reader[+A] {
  def read(params: Params): Result[A]

  def and[B](r: Reader[B]): Reader[(A, B)] = AndReader(this, r)

  def and[B, C](r1: Reader[B], r2: Reader[C]): Reader[(A, B, C)] = AndReader2(this, r1, r2)

  def and[B, C, D](r1: Reader[B], r2: Reader[C], r3: Reader[D]): Reader[(A, B, C, D)] = AndReader3(this, r1, r2, r3)

  def or[B >: A](r: Reader[B]): Reader[B] = OrReader(this, r)

  def map[B](f: A => B): Reader[B] = MapReader(this, f)

  def mapTry[B](f: A => Either[Errs, B]): Reader[B] = MapTryReader(this, f)

  def validate(p: A => Boolean, err: A => Errs): Reader[A] = FilterReader(this, p, err)

  def validate(p: A => Boolean, validation: Option[String]): Reader[A] = validate(p, Errs.validation(_, validation))

  def validate(p: A => Boolean, validation: String): Reader[A] = validate(p, Some(validation))

  def validate(p: A => Boolean): Reader[A] = validate(p, None)

  def inEnum[B >: A](set: Set[B]): Reader[B] = validate(a => set.contains(a), Errs.badEnum(_, set))

  def inEnum[B >: A](v: B, o: B*): Reader[B] = inEnum((v :: o.toList).toSet)

  def on[B](f: A => Reader[B]): Reader[(A, B)] = DynamicReader(this, f)

  def parse(args: String): Either[Errs, A] = parse(args.split(" ").map(_.trim).filter(_.nonEmpty))

  def parse(args: Array[String]): Either[Errs, A] = parse(Params(args))

  def parse(params: Params): Either[Errs, A] = read(params).toEither.map(_._1)
}

object Reader {
  def arg(pos: Int): ArgReader = ArgReader(pos)

  def argOpt(pos: Int): ArgOptReader = ArgOptReader(pos)

  def flag(name: String): FlagReader = FlagReader(name)

  def flagOpt(name: String): FlagOptReader = FlagOptReader(name)

  def flagList(name: String): FlagListReader = FlagListReader(name)

  def flagNel(name: String): FlagNelReader = FlagNelReader(name)

  def flagBool(name: String): FlagBoolReader = FlagBoolReader(name)

  def error[A](errs: Errs): ErrorReader[A] = ErrorReader[A](errs)

  def error[A](e: String): ErrorReader[A] = error(Errs.custom(e))

  case class ErrorReader[A](errs: Errs) extends Reader[A] {
    override def read(params: Params): Result[A] = Result.Failure(errs, params)
  }

  case class ArgReader(pos: Int) extends Reader[String] {
    override def read(params: Params): Result[String] = params.readArg(pos)
  }

  case class ArgOptReader(pos: Int) extends Reader[Option[String]] {
    override def read(params: Params): Result[Option[String]] = params.readArgOpt(pos)
  }

  case class FlagReader(name: String) extends Reader[String] {
    override def read(params: Params): Result[String] = params.readFlag(name)
  }

  case class FlagOptReader(name: String) extends Reader[Option[String]] {
    override def read(params: Params): Result[Option[String]] = params.readFlagOpt(name)
  }

  case class FlagListReader(name: String) extends Reader[List[String]] {
    override def read(params: Params): Result[List[String]] = params.readFlagList(name)
  }

  case class FlagNelReader(name: String) extends Reader[NonEmptyList[String]] {
    override def read(params: Params): Result[NonEmptyList[String]] = params.readFlagNel(name)
  }

  case class FlagBoolReader(name: String) extends Reader[Boolean] {
    override def read(params: Params): Result[Boolean] = params.readFlagBool(name)
  }

  case class AndReader[A, B](ra: Reader[A], rb: Reader[B]) extends Reader[(A, B)] {
    override def read(params: Params): Result[(A, B)] = ra.read(params).chain((_, p) => rb.read(p))
  }

  case class AndReader2[A, B, C](ra: Reader[A], rb: Reader[B], rc: Reader[C]) extends Reader[(A, B, C)] {
    override def read(params: Params): Result[(A, B, C)] = ra.read(params)
      .chain((_, p) => rb.read(p))
      .chain((_, p) => rc.read(p)).map { case ((a, b), c) => (a, b, c) }
  }

  case class AndReader3[A, B, C, D](ra: Reader[A], rb: Reader[B], rc: Reader[C], rd: Reader[D]) extends Reader[(A, B, C, D)] {
    override def read(params: Params): Result[(A, B, C, D)] = ra.read(params)
      .chain((_, p) => rb.read(p))
      .chain((_, p) => rc.read(p))
      .chain((_, p) => rd.read(p)).map { case (((a, b), c), d) => (a, b, c, d) }
  }

  case class OrReader[A](r1: Reader[A], r2: Reader[A]) extends Reader[A] {
    override def read(params: Params): Result[A] = r1.read(params).orElse(r2.read(params))
  }

  case class MapReader[A, B](r: Reader[A], f: A => B) extends Reader[B] {
    override def read(params: Params): Result[B] = r.read(params).map(f)
  }

  case class MapTryReader[A, B](r: Reader[A], f: A => Either[Errs, B]) extends Reader[B] {
    override def read(params: Params): Result[B] = r.read(params).flatMap((a, p) => Result.from(f(a), p))
  }

  case class FilterReader[A](r: Reader[A], f: A => Boolean, e: A => Errs) extends Reader[A] {
    override def read(params: Params): Result[A] = r.read(params).filter(f, e)
  }

  case class DynamicReader[A, B](r: Reader[A], f: A => Reader[B]) extends Reader[(A, B)] {
    override def read(params: Params): Result[(A, B)] = r.read(params).chain((a, p) => f(a).read(p))
  }

}
