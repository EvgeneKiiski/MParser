package org.mparser.benchmarks.json

import java.util.concurrent.TimeUnit

import io.circe
import io.circe.ParsingFailure
import io.circe.parser.parse
import org.mparser.MParserError
import org.mparser.benchmarks.Measure
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit}

/**
  * @author Evgenii Kiiski 
  */
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
class JsonParserTest extends JsonParser {

  @Benchmark
  def mParserSmall(): Unit = {
    objectParser.run(Examples.smallJson.toStream)
  }

  @Benchmark
  def mParserMedium(): Unit = {
    objectParser.run(Examples.mediumJson.toStream)
  }

  @Benchmark
  def mParserBig(): Unit = {
    objectParser.run(Examples.bigJson.toStream)
  }

  @Benchmark
  def circeSmall(): Unit = {
    parse(Examples.smallJson)
  }

  @Benchmark
  def circeMedium(): Unit = {
    parse(Examples.mediumJson)
  }

  @Benchmark
  def circeBig(): Unit = {
    parse(Examples.bigJson)
  }

}
