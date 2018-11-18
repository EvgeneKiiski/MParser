package org.mparser.benchmarks.json

import io.circe
import org.mparser.MParser._
import io.circe._
import io.circe.parser._
import org.mparser.{MParser, MParserError}
import org.mparser.benchmarks.{Measure, ProcessedMeasurements, Test}

import scala.io.Source

/**
  * @author Evgenii Kiiski
  */
object SimpleJsonParserTest extends JsonParser {


  def main(args: Array[String]): Unit = {

    val countTests = 100
    val measuresCount = 1000
    val measuresCountBig = 5


    val mParserSmall =
      Measure[Stream[Char], Either[MParserError, (Json, Stream[Char])]]("MParser small json", measuresCount, objectParser.run, Examples.smallJson.toStream, _.isRight)

    val mParserMedium =
      Measure[Stream[Char], Either[MParserError, (Json, Stream[Char])]]("MParser medium json", measuresCount, objectParser.run, Examples.mediumJson.toStream, _.isRight)

    val mParserBig =
      Measure[Stream[Char], Either[MParserError, (Json, Stream[Char])]]("MParser big json", measuresCountBig, objectParser.run, Examples.bigJson.toStream, _.isRight)

    val circeSmall = Measure[String, Either[ParsingFailure, circe.Json]]("Circe small json", measuresCount, parse, Examples.smallJson, _.isRight)

    val circeMedium = Measure[String, Either[ParsingFailure, circe.Json]]("Circe medium json", measuresCount, parse, Examples.mediumJson, _.isRight)

    val circeBig = Measure[String, Either[ParsingFailure, circe.Json]]("Circe big json", measuresCountBig, parse, Examples.bigJson, _.isRight)

    //val test = Test(mParserSmall, mParserMedium)
    val test = Test(mParserSmall, mParserMedium, mParserBig, circeSmall, circeMedium, circeBig)


    val result = test.run(countTests)

    result.foreach(println(_))

  }
}
