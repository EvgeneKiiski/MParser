package ru.twistedlogic.mparser

import org.scalatest.{Matchers, ParallelTestExecution, WordSpec}
import ru.twistedlogic.mparser.MParserError.EmptyStream

/**
  * @author Eugene Kiyski
  */
class MParserTest extends WordSpec with ParallelTestExecution with Matchers {

  private val abcParser = MParser.satisfy((v:Char) => v == 'a') <|> MParser.satisfy((v:Char) => v == 'b') <|> MParser.satisfy((v:Char) => v == 'c')
  private val anyParser = MParser.any[Char]()

  "MParser.satisfy" should {
    "parse empty string" in {
      val str = ""
      MParser.satisfy((v:Char) => v == 'a').run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      MParser.satisfy((v:Char) => v == 'a').run(str.toStream) shouldEqual Right(('a', Stream('a','a', 'B','B','B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      MParser.satisfy((v:Char) => v == 'a').run(str.toStream).isLeft shouldEqual true
    }
  }

  "MParser.many" should {
    "parse empty string" in {
      val str = ""
      MParser.many(anyParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream.empty))
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      MParser.many(abcParser).run(str.toStream) shouldEqual Right((Seq('a','a','a'), Stream('B','B','B')))
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      MParser.many(abcParser).run(str.toStream) shouldEqual Right((Seq('a','b','c'), Stream('B','B','B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      MParser.many(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B','B','B')))
    }
    "parse string aaa" in {
      val str = "aaa"
      MParser.many(abcParser).run(str.toStream) shouldEqual Right((Seq('a','a','a'), Stream.empty))
    }
  }

  "MParser.many1" should {
    "parse empty string" in {
      val str = ""
      MParser.many1(anyParser).run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      MParser.many1(abcParser).run(str.toStream) shouldEqual Right((Seq('a','a','a'), Stream('B','B','B')))
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      MParser.many1(abcParser).run(str.toStream) shouldEqual Right((Seq('a','b','c'), Stream('B','B','B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      MParser.many1(abcParser).run(str.toStream).isLeft shouldEqual true
    }
    "parse string aaa" in {
      val str = "aaa"
      MParser.many1(abcParser).run(str.toStream) shouldEqual Right((Seq('a','a','a'), Stream.empty))
    }
  }

  "MParser.skipMany" should {
    "parse empty string" in {
      val str = ""
      MParser.skipMany(anyParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream.empty))
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      MParser.skipMany(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B','B','B')))
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      MParser.skipMany(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B','B','B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      MParser.skipMany(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B','B','B')))
    }
    "parse string aaa" in {
      val str = "aaa"
      MParser.skipMany(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream.empty))
    }
  }

  "MParser.skipMany1" should {
    "parse empty string" in {
      val str = ""
      MParser.skipMany1(anyParser).run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string aaaBBB" in {
      val str = "aaaBBB"
      MParser.skipMany1(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B','B','B')))
    }
    "parse string abcBBB" in {
      val str = "abcBBB"
      MParser.skipMany1(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('B','B','B')))
    }
    "parse string BBB" in {
      val str = "BBB"
      MParser.skipMany1(abcParser).run(str.toStream).isLeft shouldEqual true
    }
    "parse string aaa" in {
      val str = "aaa"
      MParser.skipMany1(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream.empty))
    }
  }

  "MParser.manyTill" should {
    "parse empty string" in {
      val str = ""
      MParser.manyTill(anyParser).run(str.toStream) shouldEqual Left(EmptyStream)
    }
    "parse string BCDaaa" in {
      val str = "BCDaaa"
      MParser.manyTill(abcParser).run(str.toStream) shouldEqual Right((Seq('B','C','D'), Stream('a','a','a')))
    }
    "parse string BBBabc" in {
      val str = "BBBabc"
      MParser.manyTill(abcParser).run(str.toStream) shouldEqual Right((Seq('B','B','B'), Stream('a','b','c')))
    }
    "parse string BBB" in {
      val str = "BBB"
      MParser.manyTill(abcParser).run(str.toStream).isLeft shouldEqual true
    }
    "parse string aaa" in {
      val str = "aaa"
      MParser.manyTill(abcParser).run(str.toStream) shouldEqual Right((Seq.empty, Stream('a','a','a')))
    }
  }

}
