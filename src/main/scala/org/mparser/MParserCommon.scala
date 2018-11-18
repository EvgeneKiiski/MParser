package org.mparser

import org.mparser.MParserError.EmptyStream

import scala.collection.mutable
import scala.util.control.{BreakControl, ControlThrowable}
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
    * The parser satisfy f succeeds for any value for which the supplied function f returns True.
    * Returns the value that is actually parsed
    */
  final def satisfy[S](f: S => Boolean): MParser[S, S] = MParser { str =>
    try {
      val head = str.head
      if (f(head)) Right((head, str.tail))
      else Left(MParserError.UnexpectedSymbol(head, str.tail))
    } catch {
      case _: NoSuchElementException => leftEmptyStream
    }
  }

  /**
    * The parser succeeds for value
    */
  final def exactly[S](value: S): MParser[S, S] = MParser { str =>
    try {
      val head = str.head
      if (value == head) Right((head, str.tail))
      else Left(MParserError.UnexpectedSymbol(head, str.tail))
    } catch {
      case _: NoSuchElementException => leftEmptyStream
    }
  }

  /**
    * oneOf cs succeeds if the current value is in the supplied list of values t. Returns the parsed value.
    */
  final def oneOf[S](t: S*): MParser[S, S] = satisfy((ch: S) => t.contains(ch))

  /**
    * As the dual of oneOf, noneOf cs succeeds if the current value not in the supplied list of values t.
    * Returns the parsed character.
    */
  final def noneOf[S](t: S*): MParser[S, S] = satisfy((ch: S) => !t.contains(ch))

  /**
    * maybeOne p applies the parser p zero or one time.
    */
  final def maybeOne[S, A](p: MParser[S, A]): MParser[S, Option[A]] = MParser { str =>
    p.run(str) match {
      case Left(_) =>
        Right((None, str))
      case Right((v, tail)) =>
        Right((Some(v), tail))
    }
  }


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
      var current = str
      var builder: mutable.Builder[A, Seq[A]] = Seq.newBuilder[A]
      var result: Either[MParserError, (Seq[A], Stream[S])] = null
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

  /**
    * skipMany p applies the parser p zero or more times, skipping its result.
    */
  final def skipMany[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
    var current = str
    var continue = true
    while (continue && current.nonEmpty) {
      val result = p.run(current)
      if (result.isLeft) {
        continue = false
      } else {
        current = result.asInstanceOf[Right[MParserError, (S, Stream[S])]].value._2
      }
    }
    Right((Seq.empty, current))
  }

  /**
    * skipManyOneOf skip many of any t.
    */
  final def skipManyOneOf[S, A](t0: S): MParser[S, Seq[A]] = MParser { str =>
    var current = str
    var continue = true
    while (continue && current.nonEmpty) {
      current.head match {
        case s if t0 == s =>
          current = current.tail
        case _ =>
          continue = false
      }
    }
    Right((Seq.empty, current))
  }

  final def skipManyOneOf[S, A](t0: S, t1: S): MParser[S, Seq[A]] = MParser { str =>
    var current = str
    var continue = true
    while (continue && current.nonEmpty) {
      current.head match {
        case s if t0 == s || t1 == s =>
          current = current.tail
        case _ =>
          continue = false
      }
    }
    Right((Seq.empty, current))
  }

  final def skipManyOneOf[S, A](t0: S, t1: S, t2: S): MParser[S, Seq[A]] = MParser { str =>
    var current = str
    var continue = true
    while (continue && current.nonEmpty) {
      current.head match {
        case s if t0 == s || t1 == s || t2 == s =>
          current = current.tail
        case _ =>
          continue = false
      }
    }
    Right((Seq.empty, current))
  }

  final def skipManyOneOf[S, A](t0: S, t1: S, t2: S, t3: S): MParser[S, Seq[A]] = MParser { str =>
    var current = str
    try {
      while (true) {
        current.head match {
          case s if t0 == s || t1 == s || t2 == s || t3 == s =>
            current = current.tail
          case _ =>
            break
        }
      }
    } catch {
      case _: NoSuchElementException =>
      case _: ControlThrowable =>
    }
    Right((Seq.empty, current))
  }

  final def skipManyOneOf[S, A](t0: S, t1: S, t2: S, t3: S, ts: S*): MParser[S, Seq[A]] = MParser { str =>
    var current = str
    var continue = true
    while (continue && current.nonEmpty) {
      current.head match {
        case s if t0 == s || t1 == s || t2 == s || t3 == s || ts.contains(s) =>
          current = current.tail
        case _ =>
          continue = false
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
