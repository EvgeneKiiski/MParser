package ru.twistedlogic.mparser

/**
  * Evgenii Kiiski 
  * 16/10/2018
  */
object MParserOps {

  //TODO check
  def ˆ[A, B, C, S](a: MParser[A, S], b: MParser[B, S])(f: (A, B) => C): MParser[C, S] = {
    a.apply2(b)(f)
  }

}
