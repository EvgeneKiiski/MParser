package org.mparser

import org.scalatest.{Matchers, ParallelTestExecution, WordSpec}
import MParserError.EmptyStream

/**
  * @author Evgenii Kiiski
  */
class MParserStringTest extends WordSpec with ParallelTestExecution with Matchers {

  "MParserString.token" should {
    "parse empty string" in {
      val str = ""
      MParserString.token("äsaaa").run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      MParserString.token("abc").run(str.toStream) shouldEqual Right(("abc", Stream('B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      MParserString.token("abc").run(str.toStream).isLeft shouldEqual true
    }
  }

  "MParserString.tokenCaseInsensitive" should {
    "parse empty string" in {
      val str = ""
      MParserString.tokenCaseInsensitive("äsaaa").run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      MParserString.tokenCaseInsensitive("abc").run(str.toStream) shouldEqual Right(("abc", Stream('B', 'B', 'B')))
    }
    "parse string abcBBB case insensitive" in {
      val str = "abcBBB"
      MParserString.tokenCaseInsensitive("aBc").run(str.toStream) shouldEqual Right(("aBc", Stream('B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      MParserString.tokenCaseInsensitive("abc").run(str.toStream).isLeft shouldEqual true
    }
  }

  "MParserString.quotesString" should {
    "parse empty string" in {
      val str = ""
      MParserString.quotedString().run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse quoted string " in {
      val str = "\"bcd\""
      MParserString.quotedString().run(str.toStream) shouldEqual Right(("bcd", Stream()))
    }
    "parse quoted string with \"" in {
      val str = "\"bc\\\"d\""
      MParserString.quotedString().run(str.toStream) shouldEqual Right(("bc\\\"d", Stream()))
    }
  }

}
