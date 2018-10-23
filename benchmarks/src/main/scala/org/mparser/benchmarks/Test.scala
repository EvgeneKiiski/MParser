package org.mparser.benchmarks

/**
  * @author Evgenii Kiiski 
  */
case class Test[A, B](m: Measure[A, B]) {

  def run(n: Integer) = {
    println("start")
    var values = List.empty[Long]
    for(i <- 1 to n) {
      println(s"start $i ")
      val r = m.run()
      println(s"end $i ${r._1} ")
      values = values :+ r._1
    }
    val mean = values.sum / n
    println(s"mean=$mean")
  }

}
