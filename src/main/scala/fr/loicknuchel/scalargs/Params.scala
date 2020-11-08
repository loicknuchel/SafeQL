package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.scalargs.ArgError._
import fr.loicknuchel.scalargs.Reader.{Arg, ArgOpt, Flag, FlagBool, FlagCheck, FlagList, FlagNel, FlagOpt}

case class Params(args: List[(Int, String)], flags: Map[String, List[String]], readArgs: Set[Int], readFlags: Set[String]) {
  private def addArg(arg: String): Params = copy(args = args :+ (args.length -> arg))

  private def addFlag(flag: String): Params = copy(flags = flags + (flag -> flags.getOrElse(flag, List())))

  private def addFlagValue(flag: String, value: String): Params = copy(flags = flags + (flag -> (flags.getOrElse(flag, List()) :+ value)))

  def read(a: Arg): Result[String] = arg(a.pos, a.name).flatMap {
    case (None, p) => Result.Failure(ArgumentNotFound(a.pos, a.name, a.values), p)
    case (Some(v), p) => Result.Success(v, p)
  }

  def read(a: ArgOpt): Result[Option[String]] = arg(a.pos, a.name)

  def read(f: Flag): Result[String] = flag(f.name).flatMap {
    case (None, p) => Result.Failure(FlagNotFound(f.name), p)
    case (Some(List()), p) => Result.Failure(NoFlagValue(f.name), p)
    case (Some(List(v)), p) => Result.Success(v, p)
    case (Some(l), p) => Result.Failure(UniqueFlagHasMultipleValues(f.name, l), p)
  }

  def read(f: FlagOpt): Result[Option[String]] = flag(f.name).flatMap {
    case (None, p) => Result.Success(None, p)
    case (Some(List()), p) => Result.Failure(NoFlagValue(f.name), p)
    case (Some(List(v)), p) => Result.Success(Some(v), p)
    case (Some(l), p) => Result.Failure(UniqueFlagHasMultipleValues(f.name, l), p)
  }

  def read(f: FlagList): Result[List[String]] = flag(f.name).flatMap {
    case (Some(l), p) => Result.Success(l, p)
    case (None, p) => Result.Failure(FlagNotFound(f.name), p)
  }

  def read(f: FlagNel): Result[NonEmptyList[String]] = flag(f.name).flatMap {
    case (None, p) => Result.Failure(FlagNotFound(f.name), p)
    case (Some(List()), p) => Result.Failure(NoFlagValue(f.name), p)
    case (Some(List(v)), p) => Result.Success(NonEmptyList.of(v), p)
    case (Some(l), p) => Result.Success(NonEmptyList.fromListUnsafe(l), p)
  }

  def read(f: FlagBool): Result[Boolean] = flag(f.name).flatMap {
    case (None, p) => Result.Success(false, p)
    case (Some(List()), p) => Result.Success(true, p)
    case (Some(List(v)), p) => Result.Failure(FlagHasValue(f.name, v), p)
    case (Some(l), p) => Result.Failure(EmptyFlagHasMultipleValues(f.name, l), p)
  }

  def read(f: FlagCheck): Result[Unit] = flag(f.name).flatMap {
    case (None, p) => Result.Failure(FlagNotFound(f.name), p)
    case (Some(List()), p) => Result.Success((), p)
    case (Some(List(v)), p) => Result.Failure(FlagHasValue(f.name, v), p)
    case (Some(l), p) => Result.Failure(EmptyFlagHasMultipleValues(f.name, l), p)
  }

  private def arg(pos: Int, name: Option[String]): Result[Option[String]] =
    if (readArgs.contains(pos)) Result.Failure(ArgumentReadTwice(pos, name), this)
    else Result.Success(args.find(_._1 == pos).map(_._2), copy(readArgs = readArgs + pos))

  private def flag(name: String): Result[Option[List[String]]] =
    if (readFlags.contains(name)) Result.Failure(FlagReadTwice(name), this)
    else Result.Success(flags.get(name), copy(readFlags = readFlags + name))
}

object Params {
  def apply(args: Array[String]): Params = {
    args.foldLeft((Option.empty[String], Params(List(), Map(), Set(), Set()))) { case ((ctx, params), arg) =>
      if (arg.startsWith("--")) {
        val flag = arg.stripPrefix("--")
        (Some(flag), params.addFlag(flag))
        // TODO } else if(arg.startsWith("-")) {
      } else {
        ctx.map { c =>
          (Some(c), params.addFlagValue(c, arg))
        }.getOrElse {
          (None, params.addArg(arg))
        }
      }
    }._2
  }

  def apply(args: String): Params = Params(args.split(" "))
}
