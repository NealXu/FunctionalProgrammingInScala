package io.neal.chapter05

import org.scalatest.FlatSpec
import Stream._

class StreamTest extends FlatSpec {

  "test apis of Stream" should "be ok" in {
    val stream = Stream(1, 2, 3, 4, 5)
    val list = List(1, 2, 3, 4, 5)
    val fibsTake5 = List(0, 1, 1, 2, 3)
    val even = (x: Int) => (x % 2) == 0
    val lessThanThree = (x: Int) => x < 3
    val nonNegative = (x: Int) => x > 0
    val plusOne = (x: Int) => x + 1

    assertResult(Some(1))(stream.headOption)
    assertResult(None)(empty.headOption)
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
    assertResult(Some(1))(stream.headOption01)
    assertResult(None)(empty.headOption01)
    assertResult(list.map(plusOne))(stream.map(plusOne).toList)
    assertResult(list.filter(lessThanThree))(stream.filter(lessThanThree).toList)
    assertResult(list.flatMap(x => List(x, x)))(stream.flatMap(x => Stream(x, x)).toList)
    assertResult(list.flatMap(x => List(x, x)))(stream.flatMap01(x => Stream(x, x)).toList)
    assertResult(list ::: list.map(plusOne))(stream.append(stream.map(plusOne)).toList)
    assertResult(List(1, 1, 1))(constant(1).take(3).toList)
    assertResult(list)(from(1).take(5).toList)
    assertResult(fibsTake5)(fibs.take(5).toList)
    assertResult(List(1, 1, 1))(constant01(1).take(3).toList)
    assertResult(list)(from01(1).take(5).toList)
    assertResult(fibsTake5)(fibs01.take(5).toList)
  }

}
