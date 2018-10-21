package org.mparser

import org.scalatest.{Matchers, ParallelTestExecution, WordSpec}

/**
  * @author Evgenii Kiiski
  */
class MParserCharTest extends WordSpec with ParallelTestExecution with Matchers {

  "MParser" should {
    "parse a string into words" in {
      val text = "You can solve this problem in several different ways"
      val token = (MParserChar.spaces() >> MParser.many(MParserChar.letterOrDigit())).map(_.mkString)
      val result = MParser.many(token).run(text.toStream)
      println(result)
    }
  }

}
