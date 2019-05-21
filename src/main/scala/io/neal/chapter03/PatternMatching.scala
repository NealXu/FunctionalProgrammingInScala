package io.neal.chapter03


/**
 * chapter 3.2
 */
object PatternMatching {

  /**
   * exercise 3.1
   */
  def fetchResultOfExpression: Int = {
    List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
  }

  /**
   * exercise 3.2
   * implement the function tail
   */

  /**
   * exercise 3.3
   * implement the function setHead
   */

  /**
   * exercise 3.4
   * implement the function drop
   */

  /**
   * exercise 3.5
   * implement the function dropWhile
   */

  /**
   * exercise 3.6
   * implement the function init
   */

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
