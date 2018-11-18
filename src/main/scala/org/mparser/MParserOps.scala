package org.mparser

/**
  * @author Evgenii Kiiski
  */
private[mparser] trait MParserOps {

  def ˆ[A, B, C, S](fa: MParser[S, A], fb: MParser[S, B])(f: (A, B) => C): MParser[S, C] = MParser { str =>
    fa.run(str) match {
      case Right((a, at)) => fb.run(at) match {
        case Right((b, bt)) => Right((f(a, b), bt))
        case Left(e) => Left(e)
      }
      case Left(e) => Left(e)
    }
  }


  def ˆˆ[A, B, C, D, S](fa: MParser[S, A], fb: MParser[S, B], fc: MParser[S, C])
                       (f: (A, B, C) => D): MParser[S, D] = MParser { str =>
    fa.run(str) match {
      case Right((a, at)) => fb.run(at) match {
        case Right((b, bt)) => fc.run(bt) match {
          case Right((c, ct)) => Right((f(a, b, c), ct))
          case Left(e) => Left(e)
        }
        case Left(e) => Left(e)
      }
      case Left(e) => Left(e)
    }
  }

  def ˆˆˆ[A, B, C, D, E, S](fa: MParser[S, A], fb: MParser[S, B], fc: MParser[S, C], fd: MParser[S, D])
                           (f: (A, B, C, D) => E): MParser[S, E] = MParser { str =>
    fa.run(str) match {
      case Right((a, at)) => fb.run(at) match {
        case Right((b, bt)) => fc.run(bt) match {
          case Right((c, ct)) => fd.run(ct) match {
            case Right((d, dt)) => Right((f(a, b, c, d), dt))
            case Left(e) => Left(e)
          }
          case Left(e) => Left(e)
        }
        case Left(e) => Left(e)
      }
      case Left(e) => Left(e)
    }
  }

  def ˆˆˆˆ[A, B, C, D, E, F, S](fa: MParser[S, A], fb: MParser[S, B], fc: MParser[S, C], fd: MParser[S, D], fe: MParser[S, E])
                              (f: (A, B, C, D, E) => F): MParser[S, F] = MParser { str =>
    fa.run(str) match {
      case Right((a, at)) => fb.run(at) match {
        case Right((b, bt)) => fc.run(bt) match {
          case Right((c, ct)) => fd.run(ct) match {
            case Right((d, dt)) => fe.run(dt) match {
              case Right((e, et)) => Right((f(a, b, c, d, e), et))
              case Left(e) => Left(e)
            }
            case Left(e) => Left(e)
          }
          case Left(e) => Left(e)
        }
        case Left(e) => Left(e)
      }
      case Left(e) => Left(e)
    }
  }


}
