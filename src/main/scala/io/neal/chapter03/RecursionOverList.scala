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

  /**
   * exercise 3.13
   * Hard: Can you write foldLeft in terms of foldRight? How about the other way
   * around? Implementing foldRight via foldLeft is useful because it lets us implement
   * foldRight tail-recursively, which means it works even for large lists without overflowing
   * the stack.
   */

  /**
   * exercise 3.14
   * Implement append in terms of either foldLeft or foldRight.
   */

  /**
   * exercise 3.15
   * Hard: Write a function that concatenates a list of lists into a single list. Its runtime
   * should be linear in the total length of all lists. Try to use functions we have already
   * defined.
   */

  /**
   * exercise 3.16
   * Write a function that transforms a list of integers by adding 1 to each element.
   * (Reminder: this should be a pure function that returns a new List!)
   */

  /**
   * exercise 3.17
   * Write a function that turns each value in a List[Double] into a String. You can use
   * the expression d.toString to convert some d: Double to a String.
   */

  /**
   * exercise 3.18
   * Write a function map that generalizes modifying each element in a list while maintaining
   * the structure of the list.
   */

  /**
   * exercise 3.19
   * Write a function filter that removes elements from a list unless they satisfy a given
   * predicate. Use it to remove all odd numbers from a List[Int].
   */

  /**
   * exercise 3.20
   * Write a function flatMap that works like map except that the function given will return
   * a list instead of a single result, and that list should be inserted into the final resulting
   * list.
   */

  /**
   * exercise 3.21
   * Use flatMap to implement filter.
   */

  /**
   * exercise 3.22
   * Write a function that accepts two lists and constructs a new list by adding corresponding
   * elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
   */

  /**
   * exercise 3.23
   * Generalize the function you just wrote so that it’s not specific to integers or addition.
   * Name your generalized function zipWith.
   */

  /**
   * exercise 3.24
   * Hard: As an example, implement hasSubsequence for checking whether a List contains
   * another List as a subsequence. For instance, List(1,2,3,4) would have
   * List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
   * some difficulty finding a concise purely functional implementation that is also efficient.
   * That’s okay. Implement the function however comes most naturally. We’ll
   * return to this implementation in chapter 5 and hopefully improve on it. Note: Any
   * two values x and y can be compared for equality in Scala using the expression x == y.
   */
}
