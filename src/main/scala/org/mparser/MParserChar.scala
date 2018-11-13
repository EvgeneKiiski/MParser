package org.mparser

/**
  * @author Evgenii Kiiski
  */
private[mparser] trait MParserChar {
  this: MParserCommon =>

  /**
    * Skips zero or more white space characters
    */
  final def spaces(): MParser[Char, Seq[Char]] = many(space())

  /**
    * Space char
    */
  final def space(): MParser[Char, Char] = satisfy(_.isSpaceChar)

  /**
    * Control char
    */
  final def control(): MParser[Char, Char] = satisfy(_.isControl)

  /**
    * Upper case char
    */
  final def upper(): MParser[Char, Char] = satisfy(_.isUpper)

  /**
    * Lower case char
    */
  final def lower(): MParser[Char, Char] = satisfy(_.isLower)

  /**
    * Letter or digit
    */
  final def letterOrDigit(): MParser[Char, Char] = satisfy(_.isLetterOrDigit)

  /**
    * letter
    */
  final def letter(): MParser[Char, Char] = satisfy(_.isLetter)

  /**
    * digit
    */
  final def digit(): MParser[Char, Char] = satisfy(_.isDigit)

  /**
    * space char
    */
  final def spaceChar(): MParser[Char, Char] = satisfy(_.isSpaceChar)


  /**
    * char
    */
  final def char(c: Char): MParser[Char, Char] = exactly(c)


}
