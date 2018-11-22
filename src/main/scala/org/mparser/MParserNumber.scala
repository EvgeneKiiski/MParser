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
  final def number(delimiter: Char = '.'): MParser[Char, BigDecimal] = MParser { str =>
    var current = str
    var builder = new mutable.StringBuilder()
    var continue = true
    if (current.nonEmpty) {
      val head = current.head
      if(head.isDigit || head == '-') {
        builder += head
        current = current.tail
      } else {
        Left(UnexpectedSymbol(' ', str))
      }
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
    } else {
      Left(EmptyStream)
    }
  }

}
