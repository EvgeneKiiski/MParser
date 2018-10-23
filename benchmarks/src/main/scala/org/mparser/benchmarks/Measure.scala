package org.mparser.benchmarks

/**
  * @author Evgenii Kiiski 
  */
case class Measure[A, B](test: A => B, a: A, resultChecker: B => Boolean) {

  def run(): (Long, B) = {
    val start = System.nanoTime()
    val result = test(a)
    val end = System.nanoTime()
    if (resultChecker(result)) {
      (end - start, result)
    } else {
      println(result)
      throw new RuntimeException("wrong result")
    }
  }

}
