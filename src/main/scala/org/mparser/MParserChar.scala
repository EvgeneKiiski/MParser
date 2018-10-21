package org.mparser

/**
  * @author Evgenii Kiiski
  */
object MParserChar {

  /**
    * Skips zero or more white space characters
    */
  def spaces(): MParser[Seq[Char], Char] = MParser.many(space())

  /**
    * Space char
    */
  def space(): MParser[Char, Char] = MParser.satisfy(_.isSpaceChar)

  /**
    * Control char
    */
  def control(): MParser[Char, Char] = MParser.satisfy(_.isControl)

  /**
    * Upper case char
    */
  def upper(): MParser[Char, Char] = MParser.satisfy(_.isUpper)

  /**
    * Lower case char
    */
  def lower(): MParser[Char, Char] = MParser.satisfy(_.isLower)

  /**
    * Letter or digit
    */
  def letterOrDigit(): MParser[Char, Char] = MParser.satisfy(_.isLetterOrDigit)

  /**
    * letter
    */
  def letter(): MParser[Char, Char] = MParser.satisfy(_.isLetter)

  /**
    * digit
    */
  def digit(): MParser[Char, Char] = MParser.satisfy(_.isDigit)

  /**
    * space char
    */
  def spaceChar(): MParser[Char, Char] = MParser.satisfy(_.isSpaceChar)


  /**
    * char
    */
  def char(c: Char): MParser[Char, Char] = MParser.satisfy(_ == c)


}
