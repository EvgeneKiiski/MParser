package org.mparser

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

/**
  * @author Evgenii Kiiski 
  */
object MParsersNumberSpecification extends Properties("Number") {

  property("successful long") = forAll { b: Long =>
    MParser.number().toOption(b.toString.toStream).contains(b)
  }

}
