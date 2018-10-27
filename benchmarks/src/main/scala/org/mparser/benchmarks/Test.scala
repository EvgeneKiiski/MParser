package org.mparser.benchmarks

import org.mparser.benchmarks.json.SimpleJsonParserTest.{countTests, test}

/**
  * @author Evgenii Kiiski 
  */
case class Test(ms: Measure[_, _]*) {

  def run(n: Integer): Seq[ProcessedMeasurements] = {
    for (m <- ms) yield {
      println(s"start ${m.name}")
      var values = Seq.empty[Long]
      for (i <- 1 to n) {
        //println(s"start $i ")
        val r = m.run()
        //println(s"end $i $r ")
        values = values :+ r
      }
      ProcessedMeasurements(m.name, values)
    }
  }

}
