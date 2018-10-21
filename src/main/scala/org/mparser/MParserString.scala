package org.mparser

import org.mparser.MParserError.{EmptyStream, UnexpectedSymbol}

import scala.collection.mutable
import util.control.Breaks._


/**
  * @author Evgenii Kiiski
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

  def quotedString(): MParser[String, Char] = MParser { str =>
    if (str.isEmpty) {
      Left(EmptyStream)
    } else if (str.head != '"') {
      Left(UnexpectedSymbol(str.head, str.tail))
    } else {
      var current = str.tail
      var builder = new mutable.StringBuilder()
      var continue = true
      while (continue && current.nonEmpty) {
        current.head match {
          case '\\' if current.tail.nonEmpty =>
            builder += '\\'
            current = current.tail
            builder += current.head
            current = current.tail
          case '"' =>
            current = current.tail
            continue = false
          case s =>
            builder += s
            current = current.tail
        }
      }
      if (continue) {
        Left(UnexpectedSymbol(current.head, current.tail))
      } else {
        Right((builder.result(), current))
      }
    }
  }

}
