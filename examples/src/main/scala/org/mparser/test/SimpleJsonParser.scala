package org.mparser.test

import org.mparser.MParser
import org.mparser.MParserOps._
import org.mparser.MParser._
import org.mparser.MParserChar._
import org.mparser.MParserError.CustomError
import org.mparser.MParserString._

import scala.util.control.NonFatal

/**
  * @author Eugene Kiyski
  * It is not a json parser, it is just a example
  */
object SimpleJsonParser extends App {

  val jsonExample =
    """
      |{
      |   "firstName": "Иван",
      |   "lastName": "Иванов",
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

  val delimiter = space() <|> char(',') <|> char('\n') <|> char('\r')
  //TODO add operator not " work with escaped strings
  val key = ˆ(char('"') >> manyTill(char('"')), char('"'))((s, _) => s).map(_.mkString)

  val stringParser: MParser[Json, Char] = ˆ(char('"') >> manyTill(char('"')), char('"'))((s, _) => s)
    .map(_.mkString).map(Json.JString.apply)

  val booleanParser: MParser[Json, Char] =
    (tokenCaseInsensitive("true").`$>`(true) <|> tokenCaseInsensitive("false").`$>`(false))
      .map(Json.JBoolean.apply)

  //TODO add function parse numbers
  val numberParser: MParser[Json, Char] = many1(digit() <|> char('.')).map(_.mkString).flatMap { s =>
    try {
      pure(BigDecimal(s))
    } catch {
      case NonFatal(e) => raiseError[BigDecimal, Char](CustomError(e))
    }
  }.map(Json.JNumber.apply)

  val nullParser: MParser[Json, Char] = tokenCaseInsensitive("null").`$>`(Json.JNull)

  def keyValueParser: MParser[(String, Json), Char] = ˆˆ(
    skipMany(delimiter) >> key,
    skipMany(delimiter) >> char(':') >> skipMany(delimiter),
    stringParser <|> booleanParser <|> numberParser <|> nullParser <|> objectParser <|> arrayParser
  )((k, _, v) => k -> v)

  def objectParser: MParser[Json, Char] = ˆˆ(
    skipMany(delimiter) >> char('{'),
    many1(keyValueParser),
    skipMany(delimiter) >> char('}')
  )((_, vs, _) => vs).map(_.toMap).map(Json.JObject.apply)

  def arrayParser: MParser[Json, Char] = ˆˆ(
    skipMany(delimiter) >> char('['),
    many(skipMany(delimiter) >> (stringParser <|> booleanParser <|> numberParser <|> nullParser <|> objectParser <|> arrayParser)),
    skipMany(delimiter) >> char(']')
  )((_, vs, _) => vs).map(Json.JArray.apply)

  val result = objectParser.run(jsonExample.toStream)

  println(result)
}
