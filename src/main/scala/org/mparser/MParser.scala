package org.mparser

import org.mparser.MParserError.EmptyStream

import scala.collection.mutable

case class MParser[S, A](run: Stream[S] => Either[MParserError, (A, Stream[S])]) extends AnyVal {

  def map[B](f: A => B): MParser[S, B] = MParser { str =>
    run(str).map { case (a, tail) => (f(a), tail) }
  }

  def `$>`[B](b: B): MParser[S, B] = MParser { str =>
    run(str).map { case (_, tail) => (b, tail) }
  }

  def flatMap[B](f: A => MParser[S, B]): MParser[S, B] = MParser { str =>
    run(str).fold(Left.apply, { case (a, tail) => f(a).run(tail) })
  }

  def >>=[B](f: A => MParser[S, B]): MParser[S, B] = flatMap(f)

  def >>[B](mb: => MParser[S, B]): MParser[S, B] = flatMap(_ => mb)

  def <|>(b: => MParser[S, A]): MParser[S, A] = MParser { str =>
    run(str).fold(_ => b.run(str), Right.apply)
  }

  def handleError(f: MParserError => MParser[S, A]): MParser[S, A] = MParser { str =>
    run(str).fold(f(_).run(str), Right.apply)
  }

  def ap[B](f: => MParser[S, A => B]): MParser[S, B] = MParser { str =>
    f.run(str).flatMap { case (ab, tail) => run(tail).map { case (a, tf) => (ab(a), tf) } }
  }

  def ap2[B, C](fb: MParser[S, B])(f: => MParser[S, (A, B) => C]): MParser[S, C] =
    fb.ap(ap(f.map(_.curried)))

  def apply2[B, C](fb: MParser[S, B])(f: (A, B) => C): MParser[S, C] = ap2(fb)(MParser.pure(f))

  def tuple2[B](fb: => MParser[S, B]): MParser[S, (A, B)] =
    apply2(fb)((_, _))

  def apply3[B, C, D](fb: MParser[S, B])(fc: MParser[S, C])(f: (A, B, C) => D): MParser[S, D] =
    tuple2(fb).apply2(fc)((ab, c) => f(ab._1, ab._2, c))

}

object MParser {

  def apply[S, A](run: Stream[S] => Either[MParserError, (A, Stream[S])]): MParser[S, A] = new MParser(run)

  def pure[S, A](a: => A): MParser[S, A] = MParser(s => Right((a, s)))

  def raiseError[S, A](e: MParserError): MParser[S, A] = MParser(_ => Left(e))

  private val leftEmptyStream = Left(MParserError.EmptyStream)
  private val rightEmpty = Right((Seq.empty, Stream.empty))

  /**
    * This parser succeeds for any value. Returns the parsed value.
    */
  def any[S](): MParser[S, S] = MParser { str =>
    str.headOption.map(s => Right((s, str.tail))).getOrElse(leftEmptyStream)
  }

  /**
    * The parser satisfy f succeeds for any value for which the supplied function f returns True. Returns the value that is actually parsed
    */
  def satisfy[S](ch: S => Boolean): MParser[S, S] = MParser { str =>
    str.headOption.map {
      case s if ch(s) => Right((s, str.tail))
      case s => Left(MParserError.UnexpectedSymbol(s, str.tail))
    }.getOrElse(leftEmptyStream)
  }

  /**
    * oneOf cs succeeds if the current value is in the supplied list of values t. Returns the parsed value.
    */
  def oneOf[S](t: Traversable[S]): MParser[S, S] = satisfy((ch: S) => t.exists(_ == ch))

  /**
    * As the dual of oneOf, noneOf cs succeeds if the current value not in the supplied list of values t. Returns the parsed character.
    */
  def noneOf[S](t: Traversable[S]): MParser[S, S] = satisfy((ch: S) => !t.exists(_ == ch))


  /**
    * many p applies the parser p zero or more times. Returns a list of the returned values of p.
    */
  def many[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
    if (str.isEmpty) {
      rightEmpty
    } else {
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
  }


  /**
    * many1 p applies the parser p one or more times. Returns a list of the returned values of p.
    */
  def many1[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
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
  def skipMany[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
    if (str.isEmpty) {
      rightEmpty
    } else {
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
  }

  /**
    * skipMany1 p applies the parser p one or more times, skipping its result.
    */
  def skipMany1[S, A](p: MParser[S, A]): MParser[S, Seq[A]] = MParser { str =>
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
  def manyTill[S](p: MParser[S, S]): MParser[S, Seq[S]] = MParser { str =>
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

