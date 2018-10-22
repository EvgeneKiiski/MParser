package org.mparser

/**
  * @author Evgenii Kiiski
  */
trait MParserChar {
  this: MParserCommon =>

  /**
    * Skips zero or more white space characters
    */
  def spaces(): MParser[Char, Seq[Char]] = many(space())

  /**
    * Space char
    */
  def space(): MParser[Char, Char] = satisfy(_.isSpaceChar)

  /**
    * Control char
    */
  def control(): MParser[Char, Char] = satisfy(_.isControl)

  /**
    * Upper case char
    */
  def upper(): MParser[Char, Char] = satisfy(_.isUpper)

  /**
    * Lower case char
    */
  def lower(): MParser[Char, Char] = satisfy(_.isLower)

  /**
    * Letter or digit
    */
  def letterOrDigit(): MParser[Char, Char] = satisfy(_.isLetterOrDigit)

  /**
    * letter
    */
  def letter(): MParser[Char, Char] = satisfy(_.isLetter)

  /**
    * digit
    */
  def digit(): MParser[Char, Char] = satisfy(_.isDigit)

  /**
    * space char
    */
  def spaceChar(): MParser[Char, Char] = satisfy(_.isSpaceChar)


  /**
    * char
    */
  def char(c: Char): MParser[Char, Char] = satisfy(_ == c)


}
