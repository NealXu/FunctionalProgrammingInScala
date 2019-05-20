package io.neal.chapter02

import org.scalatest.FlatSpec

class PolymorphicFunctionTest extends FlatSpec {

  "isSorted(unsortedArray)" should "return false" in {
    val array = new Array[Int](5)
    array(0) = 3
    array(1) = 0
    array(2) = 8
    array(3) = 1
    array(4) = 7

    assert(!PolymorphicFunction.isSorted(array, (a: Int, b: Int) => a >= b))

  }

  "isSorted(sortedArray)" should "return true" in {
    val array = new Array[Long](7)
    array.indices.foreach(i => array(i) = i)

    assert(PolymorphicFunction.isSorted(array, (a: Long, b: Long) => a <= b))
  }
}
