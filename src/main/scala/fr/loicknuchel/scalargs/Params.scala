package fr.loicknuchel.scalargs

import cats.data.NonEmptyList

case class Params(args: List[(Int, String)], flags: Map[String, List[String]], readArgs: Set[Int], readFlags: Set[String]) {
  private def addArg(arg: String): Params = copy(args = args :+ (args.length -> arg))

  private def addFlag(flag: String): Params = copy(flags = flags + (flag -> flags.getOrElse(flag, List())))

  private def addFlagValue(flag: String, value: String): Params = copy(flags = flags + (flag -> (flags.getOrElse(flag, List()) :+ value)))

  def readArg(pos: Int): Result[String] = arg(pos).flatMap {
    case (None, p) => Result.Failure(Errs.argNotFound(pos), p)
    case (Some(v), p) => Result.Success(v, p)
  }

  def readArgOpt(pos: Int): Result[Option[String]] = arg(pos)

  private def arg(pos: Int): Result[Option[String]] =
    if (readArgs.contains(pos)) Result.Failure(Errs.argRead(pos), this)
    else Result.Success(args.find(_._1 == pos).map(_._2), copy(readArgs = readArgs + pos))

  def readFlag(name: String): Result[String] = flag(name).flatMap {
    case (None, p) => Result.Failure(Errs.flagNotFound(name), p)
    case (Some(List()), p) => Result.Failure(Errs.noFlagValue(name), p)
    case (Some(List(v)), p) => Result.Success(v, p)
    case (Some(l), p) => Result.Failure(Errs.multipleFlagValues(name, l), p)
  }

  def readFlagOpt(name: String): Result[Option[String]] = flag(name).flatMap {
    case (None, p) => Result.Success(None, p)
    case (Some(List()), p) => Result.Failure(Errs.noFlagValue(name), p)
    case (Some(List(v)), p) => Result.Success(Some(v), p)
    case (Some(l), p) => Result.Failure(Errs.multipleFlagValues(name, l), p)
  }

  def readFlagList(name: String): Result[List[String]] = flag(name).flatMap {
    case (Some(l), p) => Result.Success(l, p)
    case (None, p) => Result.Failure(Errs.flagNotFound(name), p)
  }

  def readFlagNel(name: String): Result[NonEmptyList[String]] = flag(name).flatMap {
    case (None, p) => Result.Failure(Errs.flagNotFound(name), p)
    case (Some(List()), p) => Result.Failure(Errs.noFlagValue(name), p)
    case (Some(List(v)), p) => Result.Success(NonEmptyList.of(v), p)
    case (Some(l), p) => Result.Success(NonEmptyList.fromListUnsafe(l), p)
  }

  def readFlagBool(name: String): Result[Boolean] = flag(name).flatMap {
    case (None, p) => Result.Success(false, p)
    case (Some(List()), p) => Result.Success(true, p)
    case (Some(List(v)), p) => Result.Failure(Errs.flagHasValue(name, v), p)
    case (Some(l), p) => Result.Failure(Errs.flagHasValues(name, l), p)
  }

  private def flag(name: String): Result[Option[List[String]]] =
    if (readFlags.contains(name)) Result.Failure(Errs.flagRead(name), this)
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
