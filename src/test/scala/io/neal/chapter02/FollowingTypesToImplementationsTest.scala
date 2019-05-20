package io.neal.chapter02

import org.scalatest.FlatSpec

class FollowingTypesToImplementationsTest extends FlatSpec {

  "test curry" should "be ok" in {
    val f = (a: Int, b: Int) => a + b
    val funcByCurry = FollowingTypesToImplementations.curry(f)

  }

}
