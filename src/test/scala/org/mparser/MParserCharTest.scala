package org.mparser

import org.scalatest.{Matchers, ParallelTestExecution, WordSpec}
import org.mparser.MParser._

/**
  * @author Evgenii Kiiski
  */
class MParserCharTest extends WordSpec with ParallelTestExecution with Matchers {

  "MParser" should {
    "parse a string into words" in {
      val text = "You can solve this problem in several different ways"
      val token = (spaces() >> many(letterOrDigit())).map(_.mkString)
      val result = many(token).run(text.toStream)
      println(result)
    }
  }

}
