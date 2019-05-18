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
}
