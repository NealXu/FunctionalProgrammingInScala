package io.neal.chapter05

import org.scalatest.FlatSpec

class StreamTest extends FlatSpec {

  "test apis of Stream" should "be ok" in {
    val stream = Stream(1, 2, 3, 4, 5)
    val list = List(1, 2, 3, 4, 5)
    val even = (x: Int) => (x % 2) == 0
    val lessThanThree = (x: Int) => x < 3
    val nonNegative = (x: Int) => x > 0

    assertResult(Some(1))(stream.headOption)
    assertResult(list)(stream.toList)
    assertResult(list)(stream.toList01)
    assertResult(list.take(3))(stream.take(3).toList)
    assertResult(list.drop(3))(stream.drop(3).toList)
    assertResult(list.takeWhile(even))(stream.takeWhile(even).toList)
    assertResult(list.takeWhile(lessThanThree))(stream.takeWhile(lessThanThree).toList)
    assertResult(true)(stream.forAll(nonNegative))
    assertResult(true)(Empty.forAll(even))
    assertResult(false)(stream.forAll(even))
    assertResult(list.reverse)(stream.reverse.toList)
    assertResult(list.takeWhile(even))(stream.takeWhile01(even).toList)
    assertResult(list.takeWhile(lessThanThree))(stream.takeWhile01(lessThanThree).toList)
  }

}
