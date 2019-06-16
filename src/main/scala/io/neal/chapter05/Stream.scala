package io.neal.chapter05

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists01(p: A => Boolean): Boolean = {
    foldRight(false)((x, y) => p(x) || y)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  /**
   * e5.1
   * Write a function to convert a Stream to a List, which will force its evaluation and let
   * you look at it in the REPL. You can convert to the regular List type in the standard
   * library. You can place this and other functions that operate on a Stream inside the
   * Stream trait.
   */
  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  def toList01: List[A] = {
    def loop(s: Stream[A], tmp: List[A]): List[A] = {
      s match {
        case Cons(h, t) => loop(t(), h() :: tmp)
        case _ => tmp
      }
    }

    loop(this, Nil).reverse
  }

  /**
   * e5.2
   * Write the function take(n) for returning the first n elements of a Stream, and
   * drop(n) for skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    def loop(s: Stream[A], tmp: Int): Stream[A] = {
      s match {
        case Cons(_, t) => if (tmp + 1 == n) t() else loop(t(), tmp + 1)
        case _ => Empty
      }
    }

    loop(this, 0)
  }

  /**
   * e5.3
   * Write the function takeWhile for returning all starting elements of a Stream that
   * match the given predicate.
   */
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }
  }

  /**
   * e5.4
   * Implement forAll, which checks that all elements in the Stream match a given predicate.
   * Your implementation should terminate the traversal as soon as it encounters a
   * non-matching value.
   */
  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) if p(h()) => t().forAll(p)
      case Cons(h, _) if !p(h()) => false
      case _ => true
    }
  }

  /**
   * e5.5
   * Use foldRight to implement takeWhile.
   */
  def takeWhile01(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else empty)
  }

  def reverse: Stream[A] = {
    def loop(s: Stream[A], tmp: Stream[A]): Stream[A] = {
      s match {
        case Cons(h, t) => loop(t(), cons(h(), tmp))
        case _ => tmp
      }
    }

    loop(this, Empty)
  }

  /**
   * e5.6
   * Hard: Implement headOption using foldRight.
   */
  def headOption01: Option[A] = {
    foldRight[Option[A]](None)((x, _) => Some(x))
  }

  /**
   * e5.7
   * Implement map, filter, append, and flatMap using foldRight. The append method
   * should be non-strict in its argument.
   */
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((x, y) => cons(f(x), y))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else y)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((x, y) => f(x).foldRight(y)((n, m) => cons(n, m)))
  }

  def flatMap01[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((x, y) => f(x).append(y))
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((x, y) => cons(x, y))
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

  /**
   * e5.8
   * Generalize ones slightly to the function constant, which returns an infinite Stream of
   * a given value.
   */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
   * e5.9
   * Write a function that generates an infinite stream of integers, starting from n, then n
   * + 1, n + 2, and so on.
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
   * e5.10
   * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
   * 2, 3, 5, 8, and so on.
   */
  def fibs: Stream[Int] = {
    def loop(x: Int, y: Int): Stream[Int] = {
      cons(x, loop(y, x + y))
    }

    loop(0, 1)
  }

}
