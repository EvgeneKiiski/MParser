package org.mparser

/**
  * @author Evgenii Kiiski
  */
private[mparser] trait MParserOps {

  def ˆ[A, B, C, S](a: MParser[S, A], b: MParser[S, B])(f: (A, B) => C): MParser[S, C] =
    a.apply2(b)(f)

  def ˆˆ[A, B, C, D, S](a: MParser[S, A], b: MParser[S, B], c: MParser[S, C])
    (f: (A, B, C) => D): MParser[S, D] = a.apply3(b)(c)(f)

}
