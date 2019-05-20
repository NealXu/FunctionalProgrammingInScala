package io.neal.chapter02

import scala.annotation.tailrec

/**
 * chapter 2.4.2
 * exercise 2.1
 */
object Fibonacci {

  def fib(n: Int): Int = {
    if (n == 1) return 1

    @tailrec
    def go(a: Int, b: Int, actualN: Int): Int = {
      val sum = a + b
      if (actualN == n) return sum
      go(b, sum, actualN + 1)
    }

    go(0, 1, 2)
  }


  def fibByRec(n: Int): Int = {
   if (n == 1 || n == 2) return 1
   fib(n - 1) + fib(n - 2)
  }

}
