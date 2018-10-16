package ru.twistedlogic.mparser.test

import ru.twistedlogic.mparser.{MParser, MParserChar, MParserError}

/**
  * @author Eugene Kiyski
  */
object Text2Tokens extends App {

  val text ="""
              You can solve this problem in several different ways.
              First: read same text
    """

  val delimeter = MParserChar.space() <|> MParserChar.char('.') <|> MParserChar.char(':')// <|> MParserChar.char('\n')

  val token = (MParser.skipMany(delimeter) >> MParser.many(MParserChar.letterOrDigit())).map(_.mkString)

  val result = MParser.many(token).run(text.toStream)

  println(result)

}
