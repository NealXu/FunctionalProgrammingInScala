package io.neal.chapter03

/**
 * chapter 3.4
 * see List.scala
 */
object RecursionOverList {

  /**
   * exercise 3.7
   * Can product, implemented using foldRight, immediately halt the recursion and
   * return 0.0 if it encounters a 0.0
   */

  /**
   * exercise 3.8
   * See what happens when you pass Nil and Cons themselves to foldRight, like this:
   * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
   */

  /**
   * exercise 3.9
   * Compute the length of a list using foldRight
   */

  /**
   * exercise 3.10
   * write another general list-recursion function, foldLeft, that is
   * tail-recursive
   */

  /**
   * exercise 3.11
   * Write sum, product, and a function to compute the length of a list using foldLeft.
   */

  /**
   * exercise 3.12
   * Write a function that returns the reverse of a list (given List(1,2,3) it returns
   * List(3,2,1)). See if you can write it using a fold.
   */


}
