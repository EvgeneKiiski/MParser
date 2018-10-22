package org.mparser

import org.mparser.MParser._
import org.mparser.MParserError.CustomError

import scala.util.control.NonFatal

/**
  * @author Evgenii Kiiski
  */
trait MParserNumber {
  this: MParserChar =>

  def number(delimiter: Char = '.'): MParser[Char, BigDecimal] = ˆˆ(
    many1(digit()),
    many(char(delimiter)),
    many(digit())
  )((a, b, c) => (a ++ b ++ c).mkString)
    .flatMap { s =>
      try {
        pure(BigDecimal(s))
      } catch {
        case NonFatal(e) => raiseError[Char, BigDecimal](CustomError(e))
      }
    }

}
