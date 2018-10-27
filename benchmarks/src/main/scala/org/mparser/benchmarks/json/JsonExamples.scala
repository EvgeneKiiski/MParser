package org.mparser.benchmarks.json

import scala.io.Source

/**
  * @author Evgenii Kiiski 
  */
trait JsonExamples {

  val smallJson =
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

  val mediumJson = Source.fromFile("/Users/evg/work/MParser/benchmarks/data/medium.json").getLines.mkString

  val bigJson = Source.fromFile("/Users/evg/work/MParser/benchmarks/data/citm_catalog.json").getLines.mkString


}
