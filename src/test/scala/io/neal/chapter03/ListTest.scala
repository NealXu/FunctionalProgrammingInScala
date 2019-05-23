package io.neal.chapter03

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "test methods of object List" should "be ok" in {
    val ls = List[Int](1 to 5: _*)
    val lsEx = List[Int](1 to 6: _*)
    val lsPlusOne = List[Int](2 to 6: _*)
    val ns = List[Double](1, 2, 3, 4, 5)
    val ms = List[String]("1", "2", "3", "4", "5")

    assertResult(List(2, 3, 4, 5))(List.tail(ls))
    assertResult(List(7, 2, 3, 4, 5))(List.setHead(7, ls))
    assertResult(List(4, 5))(List.drop(ls, 3))
    assertResult(List(3, 4, 5))(List.dropWhile01(ls, (a: Int) => a < 3))
    assertResult(List(3, 4, 5))(List.dropWhile(ls)(a => a < 3))
    assertResult(List(5, 4, 3, 2, 1))(List.reverse(ls))
    assertResult(List(1, 2, 3, 4))(List.init(ls))
    assertResult(15)(List.sum(ls))
    assertResult(120)(List.product(ns))
    assertResult(100)(List.length(List(1 to 100: _*)))
    assertResult(28)(List.foldLeft(List(1 to 10 by 2: _*), 3)(_ + _))
    assertResult(lsEx)(List.append(ls, 6))
    assertResult(List(1 to 20: _*))(List.cat(List(1 to 7: _*), List(8 to 20: _*)))
    assertResult(lsPlusOne)(List.plusOne(ls))
    assertResult(ms)(List.changeType(ns))
    assertResult(lsPlusOne)(List.map(ls)(_ + 1))
    assertResult(ms)(List.map(ns)(_.toInt.toString))
    assertResult(List[Int](2 to 5 by 2: _*))(List.filter(ls)(_ % 2 == 0))
    assertResult(List[Int](1, 1, 2, 2, 3, 3, 4, 4, 5, 5))(List.flatMap(ls)(x => List(x, x)))
    assertResult(List[Int](2 to 5 by 2: _*))(List.filter01(ls)(_ % 2 == 0))
    assertResult(List[Int](6, 6, 6, 6, 6))(List.plus(ls, List.reverse(ls)))
    assertResult(List[(Int, Int)]((1, 5), (2, 4), (3, 3), (4, 2), (5, 1)))(List.zipWith(ls, List.reverse(ls)))
    assert(List.hasSubsequence(ls, List(3, 4)))
    //assert(!List.hasSubsequence(ls, List(4, 4)))
  }

}
