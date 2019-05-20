package io.neal.chapter02

import org.scalatest.FlatSpec

class FibonacciTest extends FlatSpec {

  "fib(n)" should "output correct the n(th) fibonacci number" in {

    assertResult(1)(Fibonacci.fib(1))
    assertResult(1)(Fibonacci.fib(2))
    assertResult(2)(Fibonacci.fib(3))
    assertResult(3)(Fibonacci.fib(4))
    assertResult(5)(Fibonacci.fib(5))
    assertResult(8)(Fibonacci.fib(6))
  }

}
