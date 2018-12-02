package org.mparser.compiler

import org.mparser.compiler.MParserCompiler._

/**
  * @author Evgenii Kiiski 
  */
class MParserCompiler[A] {

  final def map[B](f: A => B): MParserCompiler[B] = Mapped(this, f)

  final def `$>`[B](b: B): MParserCompiler[B] = map(_ => b)

  final def >>[B](mb: => MParserCompiler[B]): MParserCompiler[B] = Apply2[A, B, B](this, mb, (_, b) => b)

  final def <|>(b: => MParserCompiler[A]): MParserCompiler[A] = Alternative(this, Seq(() => b))

  final def ap[B](f: => MParserCompiler[A => B]): MParserCompiler[B] = Apply2[A, A => B, B](this, f, (a, ab) => ab(a))

  final def ap2[B, C](fb: MParserCompiler[B])(f: => MParserCompiler[(A, B) => C]): MParserCompiler[C] =
    fb.ap(ap(f.map(_.curried)))

  final def apply2[B, C](fb: MParserCompiler[B])(f: (A, B) => C): MParserCompiler[C] = Apply2(this, fb, f)

  final def tuple2[B](fb: => MParserCompiler[B]): MParserCompiler[(A, B)] =
    apply2(fb)((_, _))

  final def apply3[B, C, D](fb: MParserCompiler[B])(fc: MParserCompiler[C])(f: (A, B, C) => D): MParserCompiler[D] =
    Apply3(this, fb, fc, f)

}

object MParserCompiler {

  final private[compiler] case class Suspend[A](a: MParserCommands[A]) extends MParserCompiler[A]

  final private[compiler] case class Mapped[B, C](c: MParserCompiler[C], f: C => B) extends MParserCompiler[B]

  final private[compiler] case class Alternative[A](a: MParserCompiler[A], bs: Seq[() => MParserCompiler[A]]) extends MParserCompiler[A]

  final private[compiler] case class Apply2[A, B, C](a: MParserCompiler[A], b: MParserCompiler[B], f: (A, B) => C) extends MParserCompiler[C]

  final private[compiler] case class Apply3[A, B, C, D](a: MParserCompiler[A], b: MParserCompiler[B], c: MParserCompiler[C], f: (A, B, C) => D) extends MParserCompiler[D] {
    override def toString: String = s"""Apply3(\n\t$a,\n\t$b,\n\t$c,\n\t$f)"""
  }

  //def pure[A](a: A): MParserCompiler[A] = Pure(a)

  def liftF[A](value: MParserCommands[A]): MParserCompiler[A] = Suspend(value)

}
