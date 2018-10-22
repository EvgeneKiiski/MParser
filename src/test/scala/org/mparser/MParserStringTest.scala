package org.mparser

import org.scalatest.{Matchers, ParallelTestExecution, WordSpec}
import MParserError.EmptyStream
import org.mparser.MParser._

/**
  * @author Evgenii Kiiski
  */
class MParserStringTest extends WordSpec with ParallelTestExecution with Matchers {

  "MParserString.token" should {
    "parse empty string" in {
      val str = ""
      token("äsaaa").run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      token("abc").run(str.toStream) shouldEqual Right(("abc", Stream('B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      token("abc").run(str.toStream).isLeft shouldEqual true
    }
  }

  "MParserString.tokenCaseInsensitive" should {
    "parse empty string" in {
      val str = ""
      tokenCaseInsensitive("äsaaa").run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      tokenCaseInsensitive("abc").run(str.toStream) shouldEqual Right(("abc", Stream('B', 'B', 'B')))
    }
    "parse string abcBBB case insensitive" in {
      val str = "abcBBB"
      tokenCaseInsensitive("aBc").run(str.toStream) shouldEqual Right(("aBc", Stream('B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      tokenCaseInsensitive("abc").run(str.toStream).isLeft shouldEqual true
    }
  }

  "MParserString.quotesString" should {
    "parse empty string" in {
      val str = ""
      quotedString().run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse quoted string " in {
      val str = "\"bcd\""
      quotedString().run(str.toStream) shouldEqual Right(("bcd", Stream()))
    }
    "parse quoted string with \"" in {
      val str = "\"bc\\\"d\""
      quotedString().run(str.toStream) shouldEqual Right(("bc\\\"d", Stream()))
    }
  }

}
