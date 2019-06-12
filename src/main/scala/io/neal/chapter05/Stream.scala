package io.neal.chapter05

sealed trait Stream[+A] {

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

}
