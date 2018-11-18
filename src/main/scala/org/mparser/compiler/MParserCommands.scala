package org.mparser.compiler

/**
  * @author Evgenii Kiiski 
  */
sealed trait MParserCommands[A]

object MParserCommands {

  final case class Any[S]() extends MParserCommands[S]

  final case class Satisfy[S](f: S => Boolean) extends MParserCommands[S]

  final case class Many[S](p: MParserCompiler[S]) extends MParserCommands[Seq[S]]

  import MParserCompiler._

  def any[S](): MParserCompiler[S] = liftF(Any[S]())

  def satisfy[S](f: S => Boolean): MParserCompiler[S] = liftF(Satisfy(f))

  def exactly[S](value: S): MParserCompiler[S] = liftF(Satisfy(_ == value))

  def oneOf[S](t: S*): MParserCompiler[S] = liftF(Satisfy(t.contains(_)))

  def noneOf[S](t: S*): MParserCompiler[S] = liftF(Satisfy(!t.contains(_)))

  def many[S](p: MParserCompiler[S]): MParserCompiler[Seq[S]] = liftF(Many(p))

  def quotedString(): MParserCompiler[String] =
    exactly('"')
      .apply3(many[Char](satisfy(_.isLetterOrDigit)))(exactly('"'))((_, str, _) => str.mkString)

}
