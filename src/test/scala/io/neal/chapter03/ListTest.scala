package io.neal.chapter03

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "test methods of object List" should "be ok" in {
    val ls = List[Int](1, 2, 3, 4, 5)
    assertResult(List(2, 3, 4, 5))(List.tail(ls))
    assertResult(List(7, 2, 3, 4, 5))(List.setHead(7, ls))
    assertResult(List(4, 5))(List.drop(ls, 3))
    def f(a: Int): Boolean = a < 3
    assertResult(List(3, 4, 5))(List.dropWhile(ls, f))
    assertResult(List(5, 4, 3, 2, 1))(List.reverse(ls))
    assertResult(List(1, 2, 3, 4))(List.init(ls))
  }

}
