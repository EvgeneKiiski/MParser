package ru.twistedlogic.mparser.test

import ru.twistedlogic.mparser.{MParser, MParserChar, MParserError}
import ru.twistedlogic.mparser.MParserOps._

/**
  * @author Eugene Kiyski
  */
object Text2Tokens extends App {

  val text ="""
              You can solve this problem in several different ways.
              First: read same text
    """

  val delimiter = MParserChar.space() <|> MParserChar.char('.') <|> MParserChar.char(':') <|> MParserChar.char('\n')

  val token = (MParser.skipMany1(delimiter) >> MParser.many1(MParserChar.letterOrDigit())).map(_.mkString)

  val result = MParser.many1(token).run(text.toStream)

  println(result)

}
