package org.mparser

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

/**
  * @author Evgenii Kiiski
  */
object MParsersAnySpecification extends Properties("Any") {

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

}
