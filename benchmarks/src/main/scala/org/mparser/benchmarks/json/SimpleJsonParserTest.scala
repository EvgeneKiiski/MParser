package org.mparser.benchmarks.json

import org.mparser.MParser._
import org.mparser.{MParser, MParserError}
import org.mparser.benchmarks.{Measure, Test}
import org.mparser.test.SimpleJsonParser

import scala.io.Source

/**
  * @author Evgenii Kiiski 
  */
object SimpleJsonParserTest extends App {

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

  val fileContents = Source.fromFile("/Users/evg/work/MParser/benchmarks/data/medium.json").getLines.mkString.toStream

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

  val measure =
    Measure[Stream[Char], Either[MParserError, (Json, Stream[Char])]](objectParser.run, fileContents, _.isRight)

  val test = Test(measure)

  test.run(100)



}
