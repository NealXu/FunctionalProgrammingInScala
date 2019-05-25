package io.neal.chapter03

sealed trait Tree[+A] {

  /**
   * e3.25
   * Write a function size that counts the number of nodes (leaves and branches) in a tree
   */
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  /**
   * e3.26
   * Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
   * In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
   * and y.)
   */
  def maximum(f: (A, A) => Int): A = {
    this match {
      case Leaf(v) => v
      case Branch(l, r) =>
        val leftMax = l.maximum(f)
        val rightMax = r.maximum(f)
        if (f(leftMax, rightMax) >= 0) leftMax else rightMax
    }
  }

  /**
   * e3.27
   * Write a function depth that returns the maximum path length from the root of a tree
   * to any leaf.
   */
  def depth: Int = {
    this match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + l.depth max r.depth
    }
  }

  /**
   * e3.28
   * Write a function map, analogous to the method of the same name on List, that modifies
   * each element in a tree with a given function.
   */
  def map[M, N](f: M => N): Tree[N] = {
    this match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
    }
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object Til extends Tree[Nothing]


object Tree {
  def apply[A](as: A*): Tree[A] = {
    if (as.isEmpty) {
      Til
    } else {
      as.map(Leaf(_)).foldLeft(Til: Tree[A]){(z, a) =>
        z match {
          case Til => Branch(a, Til)
          //case
        }
      }
    }
  }
}
