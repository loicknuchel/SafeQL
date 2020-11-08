package fr.loicknuchel.scalargs

import cats.data.NonEmptyList
import fr.loicknuchel.scalargs.ArgError._
import fr.loicknuchel.scalargs.Reader.{Arg, ArgOpt, Flag, FlagBool, FlagCheck, FlagList, FlagNel, FlagOpt}

// TODO alternative names: Args, Opts, Ctx
case class Params(args: List[(Int, String)], flags: Map[String, List[String]], readArgs: Set[Int], readFlags: Set[String]) {
  def size: Int = readArgs.size + readFlags.size

  def nonEmpty: Boolean = readArgs.nonEmpty || readFlags.nonEmpty

  def read(a: Arg): Result[String] = readArg(a.pos, a.name).flatMap {
    case (None, _) => Result(ArgumentNotFound(a.pos, a.name, a.values, this))
    case (Some(v), p) => Result(v, p)
  }

  def read(a: ArgOpt): Result[Option[String]] = readArg(a.pos, a.name)

  def read(f: Flag): Result[String] = readFlag(f.name).flatMap {
    case (None, _) => Result(FlagNotFound(f.name, this))
    case (Some(List()), _) => Result(NoFlagValue(f.name, this))
    case (Some(List(v)), p) => Result(v, p)
    case (Some(l), _) => Result(UniqueFlagHasMultipleValues(f.name, l, this))
  }

  def read(f: FlagOpt): Result[Option[String]] = readFlag(f.name).flatMap {
    case (None, p) => Result(None, p)
    case (Some(List()), _) => Result(NoFlagValue(f.name, this))
    case (Some(List(v)), p) => Result(Some(v), p)
    case (Some(l), _) => Result(UniqueFlagHasMultipleValues(f.name, l, this))
  }

  def read(f: FlagList): Result[List[String]] = readFlag(f.name).flatMap {
    case (Some(l), p) => Result(l, p)
    case (None, _) => Result(FlagNotFound(f.name, this))
  }

  def read(f: FlagNel): Result[NonEmptyList[String]] = readFlag(f.name).flatMap {
    case (None, _) => Result(FlagNotFound(f.name, this))
    case (Some(List()), _) => Result(NoFlagValue(f.name, this))
    case (Some(List(v)), p) => Result(NonEmptyList.of(v), p)
    case (Some(l), p) => Result(NonEmptyList.fromListUnsafe(l), p)
  }

  def read(f: FlagBool): Result[Boolean] = readFlag(f.name).flatMap {
    case (None, p) => Result(false, p)
    case (Some(List()), p) => Result(true, p)
    case (Some(List(v)), _) => Result(FlagHasValue(f.name, v, this))
    case (Some(l), _) => Result(EmptyFlagHasMultipleValues(f.name, l, this))
  }

  def read(f: FlagCheck): Result[Unit] = readFlag(f.name).flatMap {
    case (None, _) => Result(FlagNotFound(f.name, this))
    case (Some(List()), p) => Result((), p)
    case (Some(List(v)), _) => Result(FlagHasValue(f.name, v, this))
    case (Some(l), _) => Result(EmptyFlagHasMultipleValues(f.name, l, this))
  }

  def arg(a: Int*): Params = copy(readArgs = readArgs ++ a) // only for tests
  def flag(f: String*): Params = copy(readFlags = readFlags ++ f) // only for tests

  private def readArg(pos: Int, name: Option[String]): Result[Option[String]] =
    if (readArgs.contains(pos)) Result(ArgumentReadTwice(pos, name, this))
    else Result(args.find(_._1 == pos).map(_._2), arg(pos))

  private def readFlag(name: String): Result[Option[List[String]]] =
    if (readFlags.contains(name)) Result(FlagReadTwice(name, this))
    else Result(flags.get(name), flag(name))

  private def addArg(arg: String): Params = copy(args = args :+ (args.length -> arg))

  private def addFlag(flag: String): Params = copy(flags = flags + (flag -> flags.getOrElse(flag, List())))

  private def addFlagValue(flag: String, value: String): Params = copy(flags = flags + (flag -> (flags.getOrElse(flag, List()) :+ value)))
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

  def apply(args: String): Params = Params(args.split(" ").map(_.trim).filter(_.nonEmpty))
}
