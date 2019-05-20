package io.neal.chapter02

/**
 * chapter 2.6
 * exercise 2.4-2.6
 */
object FollowingTypesToImplementations {

  /* Note that since => associates to the right, A => (B => C) can be written as A => B => C. */
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => f(a, _)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

}
