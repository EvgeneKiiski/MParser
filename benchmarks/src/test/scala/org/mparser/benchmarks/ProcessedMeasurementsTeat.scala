package org.mparser.benchmarks

import org.scalatest.{Matchers, ParallelTestExecution, WordSpec}

import scala.math.{pow, sqrt}

/**
  * @author Evgenii Kiiski 
  */
class ProcessedMeasurementsTeat extends WordSpec with ParallelTestExecution with Matchers {

  "ProcessedMeasurements" should {
    "processed example" in {
      val test: Seq[Long] = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      val result = ProcessedMeasurements("test", test)

      val deviationCheck = sqrt(
        (
          pow(1 - 5, 2) +
            pow(2 - 5, 2) +
            pow(3 - 5, 2) +
            pow(4 - 5, 2) +
            pow(5 - 5, 2) +
            pow(6 - 5, 2) +
            pow(7 - 5, 2) +
            pow(8 - 5, 2) +
            pow(9 - 5, 2)) / 8
      )

      assert(result.deviation - deviationCheck < 0.000001)
      assert(result.mean == 5.0)
    }
  }

}
