package fr.loicknuchel.safeql.utils

import cats.data.NonEmptyList
import cats.effect.IO
import doobie.syntax.string._
import doobie.util.fragment.Fragment
import fr.loicknuchel.safeql.models.MultiException
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.safeql.utils.Extensions._

import scala.util.{Failure, Success, Try}

class ExtensionsSpec extends BaseSpec {
  private val e = new Exception("an error happened")
  private val e2 = new Exception("an other error happened")

  describe("Extensions") {
    describe("RichOption") {
      it("should transform to Either") {
        Option(1).toEither("2") shouldBe Right(1)
        Option.empty[Int].toEither("2") shouldBe Left("2")
        Option(1).toEither(throw e) shouldBe Right(1) // lazy param
      }
    }
    describe("RichOptionEither") {
      it("should reverse monads") {
        Some(Right(1)).sequence shouldBe Right(Some(1))
        Some(Left("s")).sequence shouldBe Left("s")
        Option.empty[Either[String, Int]].sequence shouldBe Right(None)
      }
    }
    describe("RichTry") {
      it("should transform to IO") {
        Try(1).toIO shouldBe IO.pure(1)
        Try(throw e).toIO shouldBe IO.raiseError(e)
      }
    }
    describe("RichEither") {
      it("should transform to Try") {
        Either.cond(test = true, 1, "2").asTry(_ => e) shouldBe Success(1)
        Either.cond(test = false, 1, "2").asTry(_ => e) shouldBe Failure(e)
      }
    }
    describe("RichIO") {
      it("should handle IO errors") {
        IO.pure(1).recoverWith { case _ => IO.pure(2) }.unsafeRunSync() shouldBe 1
        IO.raiseError(e).recoverWith { case _ => IO.pure(2) }.unsafeRunSync() shouldBe 2
        IO.raiseError(e).recoverWith { case `e` => IO.pure(2) }.unsafeRunSync() shouldBe 2
        an[Exception] should be thrownBy IO.raiseError(e2).recoverWith { case `e` => IO.pure(2) }.unsafeRunSync()
        an[Exception] should be thrownBy IO.raiseError(e).recoverWith { case _ => IO.raiseError(e) }.unsafeRunSync()
      }
    }
    describe("RichTraversableOnce") {
      it("should transform to NonEmptyList") {
        Seq().toNel shouldBe a[Left[_, _]]
        Seq(1).toNel shouldBe Right(NonEmptyList.of(1))
      }
      it("should join items") {
        Seq.empty[String].mk(_ + "-" + _) shouldBe None
        Seq("1").mk(_ + "-" + _) shouldBe Some("1")
        Seq("1", "2", "3").mk(_ + "-" + _) shouldBe Some("1-2-3")
      }
    }
    describe("RichTraversableOnceTry") {
      it("should reverse monads") {
        Seq(Try(1), Try(2), Try(3)).sequence shouldBe Success(Seq(1, 2, 3))
        Seq(Try(1), Try(throw e), Try(3)).sequence shouldBe Failure(e)
        Seq(Try(1), Try(throw e), Try(throw e2)).sequence shouldBe Failure(MultiException(NonEmptyList.of(e, e2))) // should keep all exceptions
      }
    }
    describe("RichTraversableOnceEither") {
      it("should reverse monads") {
        Seq[Either[String, Int]](Right(1), Right(2), Right(3)).sequence shouldBe Right(Seq(1, 2, 3))
        Seq(Right(1), Left("b"), Right(3)).sequence shouldBe Left("b")
        Seq(Right(1), Left("b"), Left("c")).sequence shouldBe Left("b")
      }
    }
    describe("RichTraversableOnceIO") {
      it("should reverse monads") {
        Seq(IO.pure(1), IO.pure(2), IO.pure(3)).sequence.unsafeRunSync() shouldBe Seq(1, 2, 3)
        Try(Seq(IO.pure(1), IO.raiseError(e), IO.pure(3)).sequence.unsafeRunSync()) shouldBe Failure(e)
        Try(Seq(IO.pure(1), IO.raiseError(e), IO.raiseError(e2)).sequence.unsafeRunSync()) shouldBe Failure(MultiException(NonEmptyList.of(e, e2))) // should keep all exceptions
      }
    }
    describe("RichTraversableOnceFragment") {
      it("should join fragments") {
        Seq.empty[Fragment].mkFragment("-").query.sql shouldBe ""
        Seq(fr0"1").mkFragment("-").query.sql shouldBe "1"
        Seq(fr0"1", fr0"2", fr0"3").mkFragment("-").query.sql shouldBe "1-2-3"
      }
    }
    describe("RichNonEmptyListFragment") {
      it("should join fragments") {
        NonEmptyList.of(fr0"1").mkFragment("-").query.sql shouldBe "1"
        NonEmptyList.of(fr0"1", fr0"2", fr0"3").mkFragment("-").query.sql shouldBe "1-2-3"
      }
    }
  }
}
