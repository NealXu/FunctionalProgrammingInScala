package io.neal.chapter03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead01[A](x: A, ls: List[A]): List[A] = ls match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(x, t)
  }

  def setHead[A](x: A, ls: List[A]): List[A] = {
    Cons(x, tail(ls))
  }

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


  def dropWhile[A](ls: List[A])(f: A => Boolean): List[A] = {
    ls match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => ls
    }
  }

  def dropWhile02[A](ls: List[A], f: A => Boolean): List[A] = {
    ls match {
      case Cons(h, t) if f(h) => dropWhile02(t, f)
      case _ => ls
    }
  }

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

  def init[A](ls: List[A]): List[A] = {
    reverse(tail(reverse(ls)))
  }


  def init01[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init01(t))
  }


  def sum(ls: List[Int]): Int = {
    foldLeft(ls, 0)(_ + _)
  }

  def sum01(ls: List[Int]): Int = {
    foldRight(ls, 0)(_ + _)
  }

  def product(ls: List[Double]): Double = {
    foldLeft(ls, 1.0)(_ * _)
  }
  def product01(ls: List[Double]): Double = {
    foldRight(ls, 1.0)(_ * _)
  }

  def length[A](ls: List[A]): Int = {
    foldLeft(ls, 0: Int)((z, _) => z + 1)
  }

  def length01[A](ls: List[A]): Int = {
    foldRight(ls, 0: Int)((_, y) => y + 1)
  }

  def foldRight[A, B](ls: List[A], z: B)(f: (A, B) => B): B = ls match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](ls: List[A], z: B)(f: (B, A) => B): B = {
    ls match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
}
