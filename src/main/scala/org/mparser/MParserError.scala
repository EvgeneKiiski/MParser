package org.mparser

/**
  * @author Evgenii Kiiski
  */
sealed trait MParserError

object MParserError {

  final case class UnexpectedSymbol[A, S](str: A, tail: Stream[S]) extends MParserError {
    override def toString: String = s"Unexpected symbol '$str' tail=$tail"
  }

  final case object EmptyStream extends MParserError {
    override def toString: String = s"Empty stream"
  }

  final case class CustomError[E](error: E) extends MParserError

}