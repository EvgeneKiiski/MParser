package org.mparser.test

import org.mparser.MParserChar._
import org.mparser.MParser._

/**
  * @author Eugene Kiyski
  */
object Text2Tokens extends App {

  val text ="""
              You can solve this problem in several different ways.
              First: read same text
    """

  val delimiter = space() <|> char('.') <|> char(':') <|> char('\n')

  val token = (skipMany1(delimiter) >> many1(letterOrDigit())).map(_.mkString)

  val result = many1(token).run(text.toStream)

  println(result)

}
