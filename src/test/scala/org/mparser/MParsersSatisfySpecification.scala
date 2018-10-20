package org.mparser

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import MParserError.{EmptyStream, UnexpectedSymbol}

/**
  * @author Eugene Kiyski
  */
object MParsersSatisfySpecification extends Properties("Satisfy") {

  property("successful") = forAll { str: String =>
    if (str.isEmpty) {
      true
    } else {
      val first = str.head
      MParser.satisfy((ch: Char) => ch == first).run(str.toStream) match {
        case Left(_) => false
        case Right((ch, _)) if ch == first => true
        case _ => false
      }
    }
  }

  property("emptyString") = forAll { c: Char =>
    MParser.satisfy((ch: Char) => ch == c).run("".toStream) match {
      case Left(EmptyStream) => true
      case Left(_) => false
      case _ => false
    }
  }

  property("unsuccessful") = forAll { str: String =>
    MParser.satisfy((ch: Char) => false).run(str.toStream) match {
      case Left(UnexpectedSymbol(_, _)) => true
      case Left(EmptyStream) if str.isEmpty => true
      case Left(_) => false
      case _ => false
    }
  }

}
