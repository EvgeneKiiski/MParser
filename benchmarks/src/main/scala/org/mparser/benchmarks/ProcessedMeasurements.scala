package org.mparser.benchmarks

import scala.math._

/**
  * @author Evgenii Kiiski 
  */
case class ProcessedMeasurements(
                                  name: String,
                                  mean: Double,
                                  deviation: Double
                                ) {
  override def toString: String = {
    val delta = round (deviation / 1000)
    val meanMicrosecond = round(mean / 1000)
    s"$name = $meanMicrosecond μs ± $delta"
  }
}

object ProcessedMeasurements {
  def apply(
             name: String,
             measurements: Seq[Long]
           ): ProcessedMeasurements = {
    val sorted = measurements.sortBy(identity)
    //throw away 10 percent of the emissions
    val size: Int = sorted.size * 9 / 10
    val realMeasurements = sorted.take(size)
    val mean: BigDecimal = realMeasurements.map(BigDecimal.apply).sum / size
    val deviation = sqrt((realMeasurements.map(BigDecimal.apply).map(x => (x - mean) * (x - mean)).sum / (size -1)).toDouble)
    new ProcessedMeasurements(name, mean.toDouble, deviation)
  }
}
