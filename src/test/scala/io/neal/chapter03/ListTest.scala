package io.neal.chapter03

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "test methods of object List" should "be ok" in {
    val ls = List[Int](1, 2, 3, 4, 5)
    val ns = List[Double](1, 2, 3, 4, 5)
    assertResult(List(2, 3, 4, 5))(List.tail(ls))
    assertResult(List(7, 2, 3, 4, 5))(List.setHead(7, ls))
    assertResult(List(4, 5))(List.drop(ls, 3))
    assertResult(List(3, 4, 5))(List.dropWhile02(ls, (a: Int) => a < 3))
    assertResult(List(3, 4, 5))(List.dropWhile(ls)(a => a < 3))
    assertResult(List(5, 4, 3, 2, 1))(List.reverse(ls))
    assertResult(List(1, 2, 3, 4))(List.init(ls))
    assertResult(15)(List.sum(ls))
    assertResult(120)(List.product(ns))
    assertResult(100)(List.length(List(1 to 100: _*)))
    assertResult(28)(List.foldLeft(List(1 to 10 by 2: _*), 3)(_ + _))
  }

}
