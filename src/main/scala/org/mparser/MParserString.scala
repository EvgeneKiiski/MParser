package org.mparser

import org.mparser.MParserError.{EmptyStream, UnexpectedSymbol}

import scala.collection.mutable
import util.control.Breaks._


/**
  * @author Evgenii Kiiski
  */
private[mparser] trait MParserString {

  private val leftEmpty = Left(EmptyStream)

  /**
    * parse string token
    */
  final def token(token: String): MParser[Char, String] = MParser { str =>
    var result: Either[MParserError, (String, Stream[Char])] = _
    var current = str
    breakable {
      try {
        for (ch <- token) {
          current.head match {
            case c if ch == c =>
              current = current.tail
            case c =>
              result = Left(UnexpectedSymbol(c, str.tail))
              break
          }
        }
      } catch {
        case _: NoSuchElementException =>
          result = leftEmpty
          break
      }
      result = Right(token, current)
    }
    result
  }

  /**
    * parse string token case insensitive
    */
  final def tokenCaseInsensitive(token: String): MParser[Char, String] = MParser { str =>
    var result: Either[MParserError, (String, Stream[Char])] = _
    var current = str
    breakable {
      try {
        for (ch <- token) {
          current.head match {
            case c if ch == c || ch.toLower == c.toLower =>
              current = current.tail
            case c =>
              result = Left(UnexpectedSymbol(c, str.tail))
              break
          }
        }
      } catch {
        case _: NoSuchElementException =>
          result = leftEmpty
          break
      }
      result = Right(token, current)
    }
    result
  }

  /**
    * parse quoted string and return string without quotes
    */
  final def quotedString(): MParser[Char, String] = MParser { str =>
    if (str.isEmpty) {
      leftEmpty
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
