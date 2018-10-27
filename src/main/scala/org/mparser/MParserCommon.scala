package org.mparser

import org.mparser.MParserError.EmptyStream

import scala.collection.mutable
import util.control.Breaks._

/**
  * @author Evgenii Kiiski 
  */
private[mparser] trait MParserCommon {

  private val leftEmptyStream = Left(MParserError.EmptyStream)

  /**
    * This parser succeeds for any value. Returns the parsed value.
    */
  final def any[S](): MParser[S, S] = MParser { str =>
    str.headOption.map(s => Right((s, str.tail))).getOrElse(leftEmptyStream)
  }

  /**
    * The parser satisfy f succeeds for any value for which the supplied function f returns True. Returns the value that is actually parsed
    */
  final def satisfy[S](ch: S => Boolean): MParser[S, S] = MParser { str =>
//    var result: Either[MParserError, (S, Stream[S])] = leftEmptyStream
//    breakable {
//      for (s <- str) {
//        if (ch(s))
//          result = Right((s, str.tail))
//        else
//          result = Left(MParserError.UnexpectedSymbol(s, str.tail))
//        break
//      }
//    }
//    result
    str.headOption.map {
      case s if ch(s) => Right((s, str.tail))
      case s => Left(MParserError.UnexpectedSymbol(s, str.tail))
    }.getOrElse(leftEmptyStream)
  }

  /**
    * oneOf cs succeeds if the current value is in the supplied list of values t. Returns the parsed value.
    */
  final def oneOf[S](t: S*): MParser[S, S] = satisfy((ch: S) => t.contains(ch))

  /**
    * As the dual of oneOf, noneOf cs succeeds if the current value not in the supplied list of values t. Returns the parsed character.
    */
  final def noneOf[S](t: S*): MParser[S, S] = satisfy((ch: S) => !t.contains(ch))


  /**
    * many p applies the parser p zero or more times. Returns a list of the returned values of p.
    */
  final def many[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
    var current = str
    var builder: mutable.Builder[A, Seq[A]] = Seq.newBuilder[A]
    var continue = true
    while (continue && current.nonEmpty) {
      p.run(current) match {
        case Left(_) =>
          continue = false
        case Right((v, tail)) =>
          current = tail
          builder += v
      }
    }
    Right((builder.result(), current))
  }


  /**
    * many1 p applies the parser p one or more times. Returns a list of the returned values of p.
    */
  final def many1[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
    if (str.isEmpty) {
      leftEmptyStream
    } else {
      var current = str
      var builder: mutable.Builder[A, Seq[A]] = Seq.newBuilder[A]
      var result: Either[MParserError, (Seq[A], Stream[S])] = Left(MParserError.EmptyStream)
      var continue = true
      while (continue) {
        p.run(current) match {
          case Left(e) =>
            continue = false
            val builderResult = builder.result()
            if (builderResult.isEmpty) {
              result = Left(e)
            } else {
              result = Right((builderResult, current))
            }
          case Right((v, tail)) =>
            current = tail
            builder += v
        }
      }
      result
    }
  }

  /**
    * skipMany p applies the parser p zero or more times, skipping its result.
    */
  final def skipMany[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
    var current = str
    var continue = true
    while (continue && current.nonEmpty) {
      p.run(current) match {
        case Left(_) => continue = false
        case Right((_, tail)) => current = tail
      }
    }
    Right((Seq.empty, current))
  }

  /**
    * skipMany1 p applies the parser p one or more times, skipping its result.
    */
  final def skipMany1[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
    if (str.isEmpty) {
      leftEmptyStream
    } else {
      var current = str
      var result: Either[MParserError, (Seq[A], Stream[S])] = leftEmptyStream
      var oneOrMore = false
      var continue = true
      while (continue) {
        p.run(current) match {
          case Left(e) =>
            continue = false
            if (oneOrMore) {
              result = Right((Seq.empty, current))
            } else {
              result = Left(e)
            }
          case Right((_, tail)) =>
            oneOrMore = true
            current = tail
        }
      }
      result
    }
  }


  /**
    * manyTill p end applies parser p zero or more times until parser end succeeds. Returns the list of values returned by p
    */
  final def manyTill[S](p: MParser[S, S]): MParser[S, Seq[S]] = MParser { str =>
    var current = str
    var builder: mutable.Builder[S, Seq[S]] = Seq.newBuilder[S]
    var result: Either[MParserError, (Seq[S], Stream[S])] = leftEmptyStream
    var continue = true
    while (current.nonEmpty && continue) {
      p.run(current) match {
        case Left(EmptyStream) =>
          val builderResult = builder.result()
          if (builderResult.isEmpty) {
            result = leftEmptyStream
          } else {
            result = Right((builderResult, current))
          }
          continue = false
        case Left(_) =>
          builder += current.head
          current = current.tail
        case Right((_, _)) =>
          result = Right((builder.result(), current))
          continue = false
      }
    }
    result
  }

}
