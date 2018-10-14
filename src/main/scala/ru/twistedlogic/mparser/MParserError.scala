package ru.twistedlogic
package mparser

sealed trait MParserError

object MParserError {

  final case class UnexpectedSymbol[A, S](str: A, tail: Stream[S]) extends MParserError

  object EmptyStream extends MParserError

  final case class CustomError[E](error: E) extends MParserError

}