package io.neal.chapter02

/**
 * chapter 2.5.1
 * exercise 2.2
 */
object PolymorphicFunction {

  def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    array.zipWithIndex.foldLeft(true){ (z, a) =>
      val currentIdx = a._2
      val currentElement = a._1
      if (currentIdx < array.length - 1) {
        val nextElement = array(currentIdx + 1)
        z && ordered(currentElement, nextElement)
      } else {
        z
      }
    }

  }

  def isSorted01[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
     for (i <- 1 until array.length) {
       if (!ordered(array(i - 1), array(i))) {
         return false
       }
     }

     true
  }

}
