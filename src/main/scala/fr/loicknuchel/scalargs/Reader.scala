package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.scalargs.ArgError.{CustomError, InvalidEnumValue, ValidationError}
import fr.loicknuchel.scalargs.Reader._

sealed trait Reader[+A] {
  def read(params: Params): Result[A]

  def read(args: Array[String]): Result[A] = read(Params(args))

  def read(args: String): Result[A] = read(Params(args))

  def and[B](r: Reader[B]): Reader[(A, B)] = And(this, r)

  def and[B, C](r1: Reader[B], r2: Reader[C]): Reader[(A, B, C)] = And2(this, r1, r2)

  def and[B, C, D](r1: Reader[B], r2: Reader[C], r3: Reader[D]): Reader[(A, B, C, D)] = And3(this, r1, r2, r3)

  def or[B >: A](r: Reader[B]): Reader[B] = this match {
    case Or(o1, o2) => Or2(o1, o2, r)
    case Or2(o1, o2, o3) => Or3(o1, o2, o3, r)
    case _ => Or(this, r)
  }

  def or[B >: A](r1: Reader[B], r2: Reader[B]): Reader[B] = this match {
    case Or(o1, o2) => Or3(o1, o2, r1, r2)
    case _ => Or2(this, r1, r2)
  }

  def or[B >: A](r1: Reader[B], r2: Reader[B], r3: Reader[B]): Reader[B] = Or3(this, r1, r2, r3)

  def map[B](f: A => B): Reader[B] = Map(this, f)

  def mapTry[B](f: (A, Params) => Either[ArgError, B]): Reader[B] = MapTry(this, f)

  def validate(p: A => Boolean, err: (A, Params) => ArgError): Reader[A] = Filter(this, p, err)

  def validate(p: A => Boolean, validation: Option[String]): Reader[A] = validate(p, ValidationError(_, validation, _))

  def validate(p: A => Boolean, validation: String): Reader[A] = validate(p, Some(validation))

  def validate(p: A => Boolean): Reader[A] = validate(p, None)

  def inEnum[B >: A](set: Set[B]): Reader[B] = validate(set.contains(_), InvalidEnumValue(_, set, _))

  def inEnum[B >: A](v: B, o: B*): Reader[B] = inEnum((v :: o.toList).toSet)

  def on[B](f: (A, Params) => Reader[B]): Reader[(A, B)] = Dynamic(this, f)
}

object Reader {
  def arg(pos: Int): Arg = Arg(pos, None, None)

  def arg(pos: Int, name: String): Arg = Arg(pos, Some(name), None)

  def argOpt(pos: Int): ArgOpt = ArgOpt(pos, None)

  def flag(name: String): Flag = Flag(name)

  def flagOpt(name: String): FlagOpt = FlagOpt(name)

  def flagList(name: String): FlagList = FlagList(name)

  def flagNel(name: String): FlagNel = FlagNel(name)

  def flagBool(name: String): FlagBool = FlagBool(name)

  def hasFlag(name: String): FlagCheck = FlagCheck(name)

  def error[A](errs: ArgError): Error[A] = Error[A](errs)

  def error[A](e: String, p: Params): Error[A] = error(CustomError(e, p))

  final case class Arg(pos: Int, name: Option[String], values: Option[Set[String]]) extends Reader[String] {
    override def read(params: Params): Result[String] = params.read(this).filter(v => values.forall(_.contains(v)), InvalidEnumValue(_, values.getOrElse(Set()), _))

    // ugly, I don't have a better way to mix covariance and String reader
    override def inEnum[B >: String](set: Set[B]): Reader[B] = Arg(pos, name, Some(set.asInstanceOf[Set[String]]))
  }

  final case class ArgOpt(pos: Int, name: Option[String]) extends Reader[Option[String]] {
    override def read(params: Params): Result[Option[String]] = params.read(this)
  }

  final case class Flag(name: String) extends Reader[String] {
    override def read(params: Params): Result[String] = params.read(this)
  }

  final case class FlagOpt(name: String) extends Reader[Option[String]] {
    override def read(params: Params): Result[Option[String]] = params.read(this)
  }

  final case class FlagList(name: String) extends Reader[List[String]] {
    override def read(params: Params): Result[List[String]] = params.read(this)
  }

  final case class FlagNel(name: String) extends Reader[NonEmptyList[String]] {
    override def read(params: Params): Result[NonEmptyList[String]] = params.read(this)
  }

  final case class FlagBool(name: String) extends Reader[Boolean] {
    override def read(params: Params): Result[Boolean] = params.read(this)
  }

  final case class FlagCheck(name: String) extends Reader[Unit] {
    override def read(params: Params): Result[Unit] = params.read(this)
  }

  final case class And[A, B](ra: Reader[A], rb: Reader[B]) extends Reader[(A, B)] {
    override def read(params: Params): Result[(A, B)] = ra.read(params).and(rb.read)
  }

  final case class And2[A, B, C](ra: Reader[A], rb: Reader[B], rc: Reader[C]) extends Reader[(A, B, C)] {
    override def read(params: Params): Result[(A, B, C)] = ra.read(params).and(rb.read).and(rc.read).map { case ((a, b), c) => (a, b, c) }
  }

  final case class And3[A, B, C, D](ra: Reader[A], rb: Reader[B], rc: Reader[C], rd: Reader[D]) extends Reader[(A, B, C, D)] {
    override def read(params: Params): Result[(A, B, C, D)] = ra.read(params).and(rb.read).and(rc.read).and(rd.read).map { case (((a, b), c), d) => (a, b, c, d) }
  }

  final case class Or[A](r1: Reader[A], r2: Reader[A]) extends Reader[A] {
    override def read(params: Params): Result[A] = r1.read(params).orElse(r2.read(params))
  }

  final case class Or2[A](r1: Reader[A], r2: Reader[A], r3: Reader[A]) extends Reader[A] {
    override def read(params: Params): Result[A] = r1.read(params).orElse(r2.read(params), r3.read(params))
  }

  final case class Or3[A](r1: Reader[A], r2: Reader[A], r3: Reader[A], r4: Reader[A]) extends Reader[A] {
    override def read(params: Params): Result[A] = r1.read(params).orElse(r2.read(params), r3.read(params), r4.read(params))
  }

  final case class Map[A, B](r: Reader[A], f: A => B) extends Reader[B] {
    override def read(params: Params): Result[B] = r.read(params).map(f)
  }

  final case class MapTry[A, B](r: Reader[A], f: (A, Params) => Either[ArgError, B]) extends Reader[B] {
    override def read(params: Params): Result[B] = r.read(params).flatMap((a, p) => f(a, p).fold(Result(_), Result(_, p)))
  }

  final case class Filter[A](r: Reader[A], f: A => Boolean, e: (A, Params) => ArgError) extends Reader[A] {
    override def read(params: Params): Result[A] = r.read(params).filter(f, e)
  }

  final case class Dynamic[A, B](r: Reader[A], f: (A, Params) => Reader[B]) extends Reader[(A, B)] {
    override def read(params: Params): Result[(A, B)] = r.read(params).chain((a, p) => f(a, p).read(p))
  }

  final case class Error[A](errs: ArgError) extends Reader[A] {
    override def read(params: Params): Result[A] = Result(errs)
  }

}
