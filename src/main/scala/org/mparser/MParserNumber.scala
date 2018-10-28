package org.mparser

import org.mparser.MParser._
import org.mparser.MParserError.{CustomError, EmptyStream, UnexpectedSymbol}

import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * @author Evgenii Kiiski
  */
private[mparser] trait MParserNumber {
  this: MParserChar =>

  /**
    * Parse number to BigDecimal
    *
    * @param delimiter - decimal separator
    */
  final def number(delimiter: Char = '.'): MParser[Char, BigDecimal] =
//      ˆˆ(
//      many1(digit()),
//      many(char(delimiter)),
//      many(digit())
//    )((a, b, c) => (a ++ b ++ c).mkString)
//      .flatMap { s =>
//        try {
//          pure(BigDecimal(s))
//        } catch {
//          case NonFatal(e) => raiseError[Char, BigDecimal](CustomError(e))
//        }
//      }
  // just test
    MParser { str =>
      var current = str
      var builder = new mutable.StringBuilder()
      var continue = true
      if(current.nonEmpty && !current.head.isDigit) {
        Left(UnexpectedSymbol(' ', str))
      } else {
        while (continue && current.nonEmpty) {
          current.head match {
            case s if s.isDigit =>
              builder += s
              current = current.tail
            case s if s == delimiter =>
              builder += delimiter
              current = current.tail
            case _ =>
              continue = false
          }
        }
        try {
          Right(BigDecimal(builder.toString()), current)
        } catch {
          case NonFatal(e) => Left(CustomError(e))
        }
      }
    }

}
