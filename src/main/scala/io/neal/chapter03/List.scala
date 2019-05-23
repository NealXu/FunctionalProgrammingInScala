package io.neal.chapter03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def head[A](ls: List[A]): Option[A] = ls match {
    case Cons(x, _) => Some(x)
    case Nil => None
  }

  def size[A](ls: List[A]): Int = {
    foldLeft(ls, 0)((b, _) =>  b + 1)
  }

  def isEmpty[A](ls: List[A]): Boolean = size(ls) == 0

  def nonEmpty[A](ls: List[A]): Boolean = !isEmpty(ls)

  // e3.2
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def tail01[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, t) => t
  }
  // e3.3
  def setHead[A](x: A, ls: List[A]): List[A] = {
    Cons(x, tail(ls))
  }

  // e3.3
  def setHead01[A](x: A, ls: List[A]): List[A] = ls match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(x, t)
  }

  // e3.4
  def drop[A](ls: List[A], n: Int): List[A] = {
    @tailrec
    def loop(ns: List[A], m: Int): List[A] = {
      ns match {
        case Nil => Nil
        case Cons(_, t) => if (m == n) t else loop(t, m + 1)
      }
    }

    loop(ls, 1)
  }


  // e3.5
  def dropWhile[A](ls: List[A])(f: A => Boolean): List[A] = {
    ls match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => ls
    }
  }

  // e3.5
  def dropWhile01[A](ls: List[A], f: A => Boolean): List[A] = {
    def loop(ns: List[A]): List[A] = {
      ns match {
        case Nil => Nil
        case Cons(h, t) => if (f(h)) loop(t) else ns
      }
    }

    loop(ls)
  }

  def reverse[A](ls: List[A]): List[A] = {
    def loop(ns: List[A], ms: List[A]): List[A] = {
      ns match {
        case Nil => ms
        case Cons(h, t) =>
          val tmp = Cons(h, ms)
          loop(t, tmp)
      }
    }

    loop(ls, Nil)
  }

  // e3.6
  def init[A](ls: List[A]): List[A] = {
    reverse(tail(reverse(ls)))
  }

  // e3.6
  def init01[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init01(t))
  }

  // e3.7
  def foldRight[A, B](ls: List[A], z: B)(f: (A, B) => B): B = ls match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // e3.9
  def length[A](ls: List[A]): Int = {
    foldLeft(ls, 0: Int)((z, _) => z + 1)
  }

  // e3.9
  def length01[A](ls: List[A]): Int = {
    foldRight(ls, 0: Int)((_, y) => y + 1)
  }

  // e3.10
  def foldLeft[A, B](ls: List[A], z: B)(f: (B, A) => B): B = {
    ls match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // e3.11
  def sum(ls: List[Int]): Int = {
    foldLeft(ls, 0)(_ + _)
  }

  // e3.11
  def sum01(ls: List[Int]): Int = {
    foldRight(ls, 0)(_ + _)
  }

  // e3.12
  def product(ls: List[Double]): Double = {
    foldLeft(ls, 1.0)(_ * _)
  }

  // e3.12
  def product01(ls: List[Double]): Double = {
    foldRight(ls, 1.0)(_ * _)
  }

  // e3.13
  def foldRight01[A, B](ls: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(ls), z)((a, b) => f(b, a))
  }

  // e3.14
  def append[A](ls: List[A], e: A): List[A] = {
    foldRight01(ls, List(e))((a, b) => Cons(a, b))
  }

  // e3.15
  def cat[A](ls: List[A], ns: List[A]): List[A] = {
    foldRight01(ls, ns)((a, b) => Cons(a, b))
  }

  // e3.16
  def plusOne(ls: List[Int]): List[Int] = {
    foldRight01(ls, Nil: List[Int])((a, b) => Cons(a + 1, b))
  }

  // e3.17
  def changeType(ls: List[Double]): List[String] = {
    foldRight01(ls, Nil: List[String])((a, b) => Cons(a.toInt.toString, b))
  }

  // e3.18
  def map[A, B](ls: List[A])(f: A => B): List[B] = {
    foldRight01(ls, Nil: List[B])((a, b) => Cons(f(a), b))
  }

  // e3.19
  def filter[A](ls: List[A])(f: A => Boolean): List[A] = {
    def loop(ns: List[A], tmp: List[A]): List[A] = ns match {
      case Nil => tmp
      case Cons(x, xs) =>
        val l = if (f(x)) Cons(x, tmp) else tmp
        loop(xs, l)
    }

    loop(reverse(ls), Nil)
  }

  // e3.20
  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] = {
    foldRight01(ls, Nil: List[B])((a, b) => cat(f(a), b))
  }

  // e3.21
  def filter01[A](ls: List[A])(f: A => Boolean): List[A] = {
    flatMap(ls)(x => if (f(x)) List(x) else Nil)
  }

  // e3.22
  def plus(ls: List[Int], ns: List[Int]): List[Int] = {
    def loop(xs: List[Int], ys: List[Int], tmp: List[Int]): List[Int] = {
      val headOfXs = head(xs)
      val headOfYs = head(ys)
      val tailOfXs = tail01(xs)
      val tailOfYs = tail01(ys)

      if (headOfXs.isDefined && headOfYs.isDefined) {
        val l = Cons(headOfXs.get + headOfYs.get, tmp)
        if (isEmpty(tailOfXs) && isEmpty(tailOfYs)) {
          l
        } else if (nonEmpty(tailOfXs) && nonEmpty(tailOfYs)) {
          loop(tailOfXs, tailOfYs, l)
        } else {
          sys.error("size of ls dose not equal sizeof ns.")
        }
      } else {
        sys.error("size of ls dose not equal sizeof ns.")
      }
    }

    loop(reverse(ls), reverse(ns), Nil)
  }

  // e3.23
  def zipWith[A, B](ls: List[A], ns: List[B]): List[(A, B)] = {
    def loop(xs: List[A], ys: List[B], tmp: List[(A, B)]): List[(A, B)] = {
      val headOfXs = head(xs)
      val headOfYs = head(ys)
      val tailOfXs = tail01(xs)
      val tailOfYs = tail01(ys)

      if (headOfXs.isDefined && headOfYs.isDefined) {
        val l = Cons((headOfXs.get, headOfYs.get), tmp)
        if (isEmpty(tailOfXs) && isEmpty(tailOfYs)) {
          l
        } else if (nonEmpty(tailOfXs) && nonEmpty(tailOfYs)) {
          loop(tailOfXs, tailOfYs, l)
        } else {
          sys.error("size of ls dose not equal sizeof ns.")
        }
      } else {
        sys.error("size of ls dose not equal sizeof ns.")
      }
    }

    loop(reverse(ls), reverse(ns), Nil)
  }

  // e.3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    true

  }

}
