package org.mparser.benchmarks

/**
  * @author Evgenii Kiiski 
  */
case class Test[A, B](m: Measure[A, B]) {

  def run(n: Integer): Seq[Long] = {
    println("start")
    var values = Seq.empty[Long]
    for(i <- 1 to n) {
      println(s"start $i ")
      val r = m.run()
      println(s"end $i $r ")
      values = values :+ r
    }
    values
  }

}
