package org.mparser

import org.mparser.MParser._
import org.mparser.MParserChar._
import org.mparser.MParserError.CustomError
import org.mparser.MParserOps.ˆˆ

import scala.util.control.NonFatal

/**
  * @author Evgenii Kiiski
  */
object MParserNumber {

  def number(delimiter: Char = '.'): MParser[BigDecimal, Char] = ˆˆ(
    many1(digit()),
    many(char(delimiter)),
    many(digit())
  )((a, b, c) => (a ++ b ++ c).mkString)
    .flatMap { s =>
      try {
        pure(BigDecimal(s))
      } catch {
        case NonFatal(e) => raiseError[BigDecimal, Char](CustomError(e))
      }
    }

}
