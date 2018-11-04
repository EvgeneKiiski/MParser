package org.mparser.benchmarks.json

import org.mparser.MParser
import org.mparser.MParser._
import org.openjdk.jmh.annotations.{Scope, State}

/**
  * @author Evgenii Kiiski 
  */
@State(Scope.Benchmark)
abstract class JsonParser {

  import Json._

  val delimiter = oneOf(' ', ',', '\n', '\r')

  val manyDelimiters = skipManyOneOf(' ', ',', '\n', '\r')

  val key = quotedString()

  val stringParser: MParser[Char, Json] = quotedString().map(JString.apply)

  val booleanParser: MParser[Char, Json] =
    (tokenCaseInsensitive("true").`$>`(true) <|> tokenCaseInsensitive("false").`$>`(false))
      .map(JBoolean.apply)

  val numberParser: MParser[Char, Json] = number().map(JNumber.apply)

  val nullParser: MParser[Char, Json] = tokenCaseInsensitive("null").`$>`(JNull)

  lazy val anyJsonParser: MParser[Char, Json] = manyDelimiters >>
    (stringParser <|> numberParser <|> nullParser <|> booleanParser <|> objectParser <|> arrayParser)

  lazy val keyValueParser: MParser[Char, (String, Json)] = manyDelimiters >> ˆˆ(
    key,
    manyDelimiters >> char(':'),
    anyJsonParser
  )((k, _, v) => k -> v)

  lazy val objectParser: MParser[Char, Json] = manyDelimiters >> ˆˆ(
    char('{'),
    many(keyValueParser),
    manyDelimiters >> char('}')
  )((_, vs, _) => vs).map(_.toMap).map(JObject.apply)

  lazy val  arrayParser: MParser[Char, Json] = ˆˆ(
    char('['),
    many(anyJsonParser),
    manyDelimiters >> char(']')
  )((_, vs, _) => vs).map(JArray.apply)

}
