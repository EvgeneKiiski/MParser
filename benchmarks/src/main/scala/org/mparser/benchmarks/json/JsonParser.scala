package org.mparser.benchmarks.json

import org.mparser.MParser
import org.mparser.MParser._

/**
  * @author Evgenii Kiiski 
  */
trait JsonParser {

  sealed trait Json

  object Json {

    case class JString(value: String) extends Json

    case class JBoolean(value: Boolean) extends Json

    case class JNumber(value: BigDecimal) extends Json

    case object JNull extends Json

    case class JObject(value: Map[String, Json]) extends Json

    case class JArray(value: Seq[Json]) extends Json

  }

  import Json._

  val delimiter = oneOf(' ', ',', '\n', '\r')

  val key = quotedString()

  val stringParser: MParser[Char, Json] = quotedString().map(JString.apply)

  val booleanParser: MParser[Char, Json] =
    (tokenCaseInsensitive("true").`$>`(true) <|> tokenCaseInsensitive("false").`$>`(false))
      .map(JBoolean.apply)

  val numberParser: MParser[Char, Json] = number().map(JNumber.apply)

  val nullParser: MParser[Char, Json] = tokenCaseInsensitive("null").`$>`(JNull)

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
  )((_, vs, _) => vs).map(_.toMap).map(JObject.apply)

  def arrayParser: MParser[Char, Json] = ˆˆ(
    skipMany(delimiter) >> char('['),
    many(skipMany(delimiter) >> anyJsonParser),
    skipMany(delimiter) >> char(']')
  )((_, vs, _) => vs).map(JArray.apply)

}
