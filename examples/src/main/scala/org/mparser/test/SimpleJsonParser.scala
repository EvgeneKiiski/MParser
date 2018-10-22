package org.mparser.test

import org.mparser.MParser
import org.mparser.MParser._


/**
  * @author Evgenii Kiiski
  *         It is not a json parser, it is just a example
  */
object SimpleJsonParser extends App {

  val jsonExample =
    """
      |{
      |   "firstName": "Иван",
      |   "lastName": "Иван\"ов",
      |   "isDone": true,
      |   "address": {
      |       "streetAddress": "Московское ш., 101, кв.101",
      |       "city": "Ленинград",
      |       "postalCode": 101101
      |   },
      |   "phoneNumbers": [
      |               "812 123-1234",
      |               "916 123-4567"
      |           ]
      |}
    """.stripMargin


  sealed trait Json

  object Json {

    case class JString(value: String) extends Json

    case class JBoolean(value: Boolean) extends Json

    case class JNumber(value: BigDecimal) extends Json

    object JNull extends Json

    case class JObject(value: Map[String, Json]) extends Json

    case class JArray(value: Seq[Json]) extends Json

  }

  val delimiter = oneOf(' ', ',', '\n', '\r')

  val key = quotedString()

  val stringParser: MParser[Char, Json] = quotedString().map(Json.JString.apply)

  val booleanParser: MParser[Char, Json] =
    (tokenCaseInsensitive("true").`$>`(true) <|> tokenCaseInsensitive("false").`$>`(false))
      .map(Json.JBoolean.apply)

  val numberParser: MParser[Char, Json] = number().map(Json.JNumber.apply)

  val nullParser: MParser[Char, Json] = tokenCaseInsensitive("null").`$>`(Json.JNull)

  def anyJsonParser: MParser[Char, Json] =
    stringParser <|> booleanParser <|> numberParser <|> nullParser <|> objectParser <|> arrayParser

  def keyValueParser: MParser[Char, (String, Json)] = ˆˆ(
    skipMany(delimiter) >> key,
    skipMany(delimiter) >> char(':') >> skipMany(delimiter),
    anyJsonParser
  )((k, _, v) => k -> v)

  def objectParser: MParser[Char, Json] = ˆˆ(
    skipMany(delimiter) >> char('{'),
    many1(keyValueParser),
    skipMany(delimiter) >> char('}')
  )((_, vs, _) => vs).map(_.toMap).map(Json.JObject.apply)

  def arrayParser: MParser[Char, Json] = ˆˆ(
    skipMany(delimiter) >> char('['),
    many(skipMany(delimiter) >> anyJsonParser),
    skipMany(delimiter) >> char(']')
  )((_, vs, _) => vs).map(Json.JArray.apply)

  val result = objectParser.run(jsonExample.toStream)

  println(result)
}
