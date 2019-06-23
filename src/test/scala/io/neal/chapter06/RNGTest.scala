package io.neal.chapter06

import org.scalatest.FlatSpec

class RNGTest extends FlatSpec {

  import RNG._

  it should "be ok" in {
    val rng = SimpleRNG(777)
    val (n1, rng1) = nonNegativeInt(rng)
    val (n2, rng2) = double(rng1)
    assertResult(true)(n1 >=0 && n1 <= Int.MaxValue)
    assertResult(true)(n2 >= 0d && n2 < 1d)
  }

}
