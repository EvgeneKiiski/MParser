package org.mparser.benchmarks.json

import org.mparser.MParser._
import org.mparser.{MParser, MParserError}
import org.mparser.benchmarks.{Measure, ProcessedMeasurements, Test}

import scala.io.Source

/**
  * @author Evgenii Kiiski 
  */
object SimpleJsonParserTest extends App with JsonParser {



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


  val result = ProcessedMeasurements("medium", test.run(1000))

  println(result)



}
