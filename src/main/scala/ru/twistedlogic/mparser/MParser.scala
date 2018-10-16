package ru.twistedlogic
package mparser

import ru.twistedlogic.mparser.MParserError.EmptyStream

import scala.collection.mutable

case class MParser[A, S](run: Stream[S] => Either[MParserError, (A, Stream[S])]) extends AnyVal {

  def map[B](f: A => B): MParser[B, S] = MParser { str =>
    run(str).map { case (a, tail) => (f(a), tail) }
  }

  def flatMap[B](f: A => MParser[B, S]): MParser[B, S] = MParser { str =>
    run(str).fold(Left.apply, { case (a, tail) => f(a).run(tail) })
  }

  def >>=[B](f: A => MParser[B, S]): MParser[B, S] = flatMap(f)

  def >>[B](mb: => MParser[B, S]): MParser[B, S] = flatMap(_ => mb)

  def <|>(b: MParser[A, S]): MParser[A, S] = MParser { str =>
    run(str).fold(_ => b.run(str), Right.apply)
  }

  def handleError(f: MParserError => MParser[A, S]): MParser[A, S] = MParser { str =>
    run(str).fold(f(_).run(str), Right.apply)
  }

  def *>[B](mb: => MParser[B, S]): MParser[B, S] = MParser { str =>
    run(str).fold(Left.apply, { case (_, tail) => mb.run(tail) })
  }

  def ap[B](f: MParser[A => B, S]): MParser[B, S] = MParser { str =>
    run(str).flatMap { case (a, tail) => f.run(tail).map { case (ab, tf) => (ab(a), tf) } }
  }

  def ap2[B, C](fb: MParser[B, S])(f: MParser[(A, B) => C, S]): MParser[C, S] =
    fb.ap(ap(f.map(_.curried)))

  def apply2[B, C](fb: MParser[B, S])(f: (A, B) => C): MParser[C, S] = ap2(fb)(MParser.pure(f))

  def tuple2[B](fb: => MParser[B, S]): MParser[(A, B), S] =
    apply2(fb)((_,_))

  def apply3[B, C, D](fb: MParser[B, S])(fc: MParser[C, S])(f: (A, B, C) => D): MParser[D, S] =
    tuple2(fb).apply2(fc)((ab, c) => f(ab._1, ab._2, c))

}

object MParser {

  def apply[A, S](run: Stream[S] => Either[MParserError, (A, Stream[S])]): MParser[A, S] = new MParser(run)

  def pure[A, S](a: => A): MParser[A, S] = MParser(s => Right((a, s)))

  def raiseError[A, S](e: MParserError): MParser[A, S] = MParser(_ => Left(e))

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
      case s => Left(MParserError.UnexpectedSymbol(s"Unexpected symbol $s", str.tail))
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
  def many[A, S](p: MParser[A, S]): MParser[Seq[A], S] = MParser { str =>
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
            //TODO
            println(s"add=$v tail=$tail")
        }
      }
      Right((builder.result(), current))
    }
  }


  /**
    * many1 p applies the parser p one or more times. Returns a list of the returned values of p.
    */
  def many1[A, S](p: MParser[A, S]): MParser[Seq[A], S] = MParser { str =>
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
  def skipMany[A, S](p: MParser[A, S]): MParser[Seq[A], S] = MParser { str =>
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
  def skipMany1[A, S](p: MParser[A, S]): MParser[Seq[A], S] = MParser { str =>
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
  def manyTill[S](p: MParser[S, S]): MParser[Seq[S], S] = MParser { str =>
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

