package org.mparser.benchmarks

/**
  * @author Evgenii Kiiski 
  */
case class Measure[A, B](test: A => B, a: A, resultChecker: B => Boolean) {

  val count = 100

  def run(): Long= {
    val start = System.nanoTime()
    val result = (1 to count).map(_ => test(a))
    val end = System.nanoTime()

    if (result.forall(r => resultChecker(r))) {
      (end - start) / count
    } else {
      println(result)
      throw new RuntimeException("wrong result")
    }
  }

}
