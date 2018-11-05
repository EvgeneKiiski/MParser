package org.mparser.benchmarks.json

import org.openjdk.jmh.annotations.{Scope, State}

import scala.io.Source

/**
  * @author Evgenii Kiiski 
  */
@State(Scope.Benchmark)
object Examples {

  val smallJson: String =
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

  val mediumJson: String = Source.fromFile("/Users/evg/work/MParser/benchmarks/data/medium.json").getLines.mkString

  val bigJson: String = Source.fromFile("/Users/evg/work/MParser/benchmarks/data/citm_catalog.json").getLines.mkString

}
