package org.mparser


class MParser[S, A](val run: Stream[S] => Either[MParserError, (A, Stream[S])]) extends AnyVal {

  def map[B](f: A => B): MParser[S, B] = MParser { str =>
    run(str).map { case (a, tail) => (f(a), tail) }
  }

  def `$>`[B](b: B): MParser[S, B] = map(_ => b)

  def flatMap[B](f: A => MParser[S, B]): MParser[S, B] = MParser { str =>
    run(str).fold(Left.apply, { case (a, tail) => f(a).run(tail) })
  }

  def >>=[B](f: A => MParser[S, B]): MParser[S, B] = flatMap(f)

  def >>[B](mb: => MParser[S, B]): MParser[S, B] = flatMap(_ => mb)

  /**
    * Alternative operator, return the first successful parse
    */
  def <|>(b: => MParser[S, A]): MParser[S, A] = MParser { str =>
    val result = run(str)
    if(result.isRight) {
      result
    } else {
      b.run(str)
    }
    //run(str).fold(_ => b.run(str), Right.apply)
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

object MParser
  extends MParserCommon
    with MParserChar
    with MParserNumber
    with MParserString
    with MParserOps {

  final def apply[S, A](run: Stream[S] => Either[MParserError, (A, Stream[S])]): MParser[S, A] = new MParser(run)

  final def pure[S, A](a: => A): MParser[S, A] = MParser(s => Right((a, s)))

  final def raiseError[S, A](e: MParserError): MParser[S, A] = MParser(_ => Left(e))

}

