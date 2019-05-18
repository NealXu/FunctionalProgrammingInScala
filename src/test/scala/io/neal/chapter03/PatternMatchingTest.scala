package io.neal.chapter03

import org.scalatest.FlatSpec

class PatternMatchingTest extends FlatSpec {

  "result of expression" should "be 3" in {
    assertResult(3)(PatternMatching.fetchResultOfExpression)
  }

}
