package org.mparser

import org.scalatest.{Matchers, ParallelTestExecution, WordSpec}
import MParserError.EmptyStream
import org.mparser.MParser._

/**
  * @author Evgenii Kiiski
  */
class MParserTest extends WordSpec with ParallelTestExecution with Matchers {

  private val abcParser: MParser[Char, Char] = MParser.satisfy((v: Char) => v == 'a') <|> MParser.satisfy((v: Char) => v == 'b') <|> MParser.satisfy((v: Char) => v == 'c')
  private val anyParser = MParser.any[Char]()

  "MParser.satisfy" should {
    "parse empty string" in {
      val str = ""
      satisfy((v: Char) => v == 'a').run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      satisfy((v: Char) => v == 'a').run(str.toStream) shouldEqual Right(('a', Stream('a', 'a', 'B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      satisfy((v: Char) => v == 'a').run(str.toStream).isLeft shouldEqual true
    }
  }

  "MParser.many" should {
    "parse empty string" in {
      val str = ""
      many(anyParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream.empty))
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      many(abcParser).run(str.toStream) shouldEqual Right((Seq('a', 'a', 'a'), Stream('B', 'B', 'B')))
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      many(abcParser).run(str.toStream) shouldEqual Right((Seq('a', 'b', 'c'), Stream('B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      many(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B', 'B', 'B')))
    }
    "parse string aaa" in {
      val str = "aaa"
      many(abcParser).run(str.toStream) shouldEqual Right((Seq('a', 'a', 'a'), Stream.empty))
    }
  }

  "MParser.many1" should {
    "parse empty string" in {
      val str = ""
      many1(anyParser).run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      many1(abcParser).run(str.toStream) shouldEqual Right((Seq('a', 'a', 'a'), Stream('B', 'B', 'B')))
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      many1(abcParser).run(str.toStream) shouldEqual Right((Seq('a', 'b', 'c'), Stream('B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      many1(abcParser).run(str.toStream).isLeft shouldEqual true
    }
    "parse string aaa" in {
      val str = "aaa"
      many1(abcParser).run(str.toStream) shouldEqual Right((Seq('a', 'a', 'a'), Stream.empty))
    }
  }

  "MParser.skipMany" should {
    "parse empty string" in {
      val str = ""
      skipMany(anyParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream.empty))
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      skipMany(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B', 'B', 'B')))
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      skipMany(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      skipMany(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B', 'B', 'B')))
    }
    "parse string aaa" in {
      val str = "aaa"
      skipMany(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream.empty))
    }
  }

  "MParser.skipMany1" should {
    "parse empty string" in {
      val str = ""
      skipMany1(anyParser).run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      skipMany1(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B', 'B', 'B')))
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      skipMany1(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B', 'B', 'B')))
    }
    "parse string BBB" in {
      val str =
        """
        BBB
                """
      skipMany1(abcParser).run(str.toStream).isLeft shouldEqual true
    }
    "parse string aaa" in {
      val str = "aaa"
      skipMany1(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream.empty))
    }
    "parse string n aaa" in {
      val str = "\naaa"
      skipMany1(abcParser).run(str.toStream).isLeft shouldEqual true
    }
  }

  "MParser.manyTill" should {
    "parse empty string" in {
      val str = ""
      manyTill(anyParser).run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string BCDaaa" in {
      val str = "BCDaaa"
      manyTill(abcParser).run(str.toStream) shouldEqual Right((Seq('B', 'C', 'D'), Stream('a', 'a', 'a')))
    }
    "parse string BBBabc" in {
      val str = "BBBabc"
      manyTill(abcParser).run(str.toStream) shouldEqual Right((Seq('B', 'B', 'B'), Stream('a', 'b', 'c')))
    }
    "parse string BBB" in {
      val str = "BBB"
      manyTill(abcParser).run(str.toStream).isLeft shouldEqual true
    }
    "parse string aaa" in {
      val str = "aaa"
      manyTill(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('a', 'a', 'a')))
    }
  }

  "MParser.apply2" should {
    "parse empty string" in {
      val str = ""
      abcParser.apply2(abcParser) { case (a, b) => "" + a + b }.run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string aaaBBB" in {
      val str = "abcBBB"
      abcParser.apply2(abcParser) { case (a, b) => "" + a + b }.run(str.toStream) shouldEqual Right(("ab", Stream('c', 'B', 'B', 'B')))
    }

  }

}
