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

  /**
   * e5.13
   * Write fibs, from, constant, and ones in terms of unfold.
   */
  def map01[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }
  }

  def take01(n: Int): Stream[A] = {
    unfold((this, n)) { s =>
      s._1 match {
        case Cons(h, t) => if (s._2 > 0) Some(h(), (t(), s._2 - 1)) else None
        case Empty => None
      }
    }
  }

  def takeWhile02(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) => if (p(h())) Some(h(), t()) else None
      case Empty => None
    }
  }

  def zipWith[B](s: Stream[B]): Stream[(A, B)] = {
    unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((h(), hh()), (t(), tt()))
      case _ => None
    }
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((Some(h()), Some(hh())), (t(), tt()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case _ => None
    }
  }

  /**
   * e5.14
   * Hard: Implement startsWith using functions you’ve written. It should check if one
   * Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
   * would be true.
   */
  def startsWith[B >: A](s: Stream[B]): Boolean = {
    this.tails.exists { x =>
      def loop(n: Stream[A], m: Stream[B]): Boolean = {
        (n, m) match {
          case (Cons(h, t), Cons(hh, tt)) => if (h() == hh()) loop(t(), tt()) else false
          case (_, Empty) => true
          case _ => false
        }
      }
      loop(x, s)
    }
  }

  /**
   * e5.15
   * Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes
   * of the input sequence, starting with the original Stream. For example, given
   * Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3),
   * Stream()).
   */
  def tail: Stream[A] = {
    this match {
      case Cons(_, t) => t()
      case _ => Empty
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s: Stream[A] => Some(s, s.tail)
    }
  }

  /**
   * e5.16
   * Hard: Generalize tails to the function scanRight, which is like a foldRight that
   * returns a stream of the intermediate results. For example:
   * scala> Stream(1,2,3).scanRight(0)(_ + _).toList
   * res0: List[Int] = List(6,5,3,0)
   */
  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
    unfold(this) {
      case Empty => None
      case s: Stream[A] => Some(s.foldRight(z)((x, y) => f(x, y)), s.tail)
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

  /**
   * e5.11
   * Write a more general stream-building function called unfold. It takes an initial state,
   * and a function for producing both the next state and the next value in the generated
   * stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some(x) => cons(x._1, unfold(x._2)(f))
      case None => empty[A]
    }
  }

  /**
   * e5.12
   * Write fibs, from, constant, and ones in terms of unfold.
   */
  def constant01[A](a: A): Stream[A] = {
    unfold(a)(Some(a, _))
  }

  def from01(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s + 1))
  }

  def fibs01: Stream[Int] = {
    unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
  }

}
