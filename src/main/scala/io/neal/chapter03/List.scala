package io.neal.chapter03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def tail[A](ls: List[A]): List[A] = ls match {
    //case Nil => Nil
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](x: A, ls: List[A]): List[A] = {
    //Cons(x, tail(ls))
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(x, t)
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


  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = {
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

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
}
