package fr.loicknuchel.safeql.utils

import cats.data.NonEmptyList

case class MultiException(errs: NonEmptyList[Throwable]) extends RuntimeException {
  override def getMessage: String = errs.toList.map(e => s"\n  - ${e.getMessage}").mkString

  override def getLocalizedMessage: String = errs.toList.map(e => s"\n  - ${e.getLocalizedMessage}").mkString

  override def getStackTrace: Array[StackTraceElement] = errs.head.getStackTrace

  override def getCause: Throwable = errs.head.getCause
}

object MultiException {
  def apply(err: Throwable, others: Throwable*): MultiException = new MultiException(NonEmptyList(err, others.toList))
}
