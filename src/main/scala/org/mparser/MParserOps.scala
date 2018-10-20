package org.mparser

/**
  * @author Evgenii Kiiski
  */
object MParserOps {

  def ˆ[A, B, C, S](a: MParser[A, S], b: MParser[B, S])(f: (A, B) => C): MParser[C, S] =
    a.apply2(b)(f)

  def ˆˆ[A, B, C, D, S](a: MParser[A, S], b: MParser[B, S], c: MParser[C, S])
    (f: (A, B, C) => D): MParser[D, S] = a.apply3(b)(c)(f)

}
