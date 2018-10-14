package ru.twistedlogic.mparser

import ru.twistedlogic.mparser.MParser.leftEmptyStream
import ru.twistedlogic.mparser.MParserError.{EmptyStream, UnexpectedSymbol}

import util.control.Breaks._


/**
  * @author Eugene Kiyski
  */
object MParserString {

  def token(token: String): MParser[String, Char] = MParser { str =>
    var result: Either[MParserError, (String, Stream[Char])] = Left(EmptyStream)
    var current = str
    breakable {
      for (ch <- token) {
        current.headOption match {
          case Some(c) if ch == c =>
            current = current.tail
          case Some(c) =>
            result = Left(UnexpectedSymbol(c, str.tail))
            break
          case None =>
            result = Left(EmptyStream)
            break
        }
      }
      result = Right(token, current)
    }
    result
  }

  def tokenCaseInsensitive(token: String): MParser[String, Char] = MParser { str =>
    var result: Either[MParserError, (String, Stream[Char])] = Left(EmptyStream)
    var current = str
    breakable {
      for (ch <- token) {
        current.headOption match {
          case Some(c) if ch.toLower == c.toLower =>
            current = current.tail
          case Some(c) =>
            result = Left(UnexpectedSymbol(c, str.tail))
            break
          case None =>
            result = Left(EmptyStream)
            break
        }
      }
      result = Right(token, current)
    }
    result
  }

}
