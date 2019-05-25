package io.neal.chapter04

import scala.math.pow

object OptionDataType {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None else Some(xs.sum / xs.size)
  }

  /**
   * e4.2
   * Implement the variance function in terms of flatMap. If the mean of a sequence is m,
   * the variance is the mean of math.pow(x - m, 2) for each element x in the sequence
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) match {
      case None => None
      case Some(m) => mean(xs.map(y => pow(y - m, 2)))
    }
  }


  def variance01(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] = try Some(a) catch { case _: Exception => None }


  /**
   * e4.3
   * Write a generic function map2 that combines two Option values using a binary function.
   * If either Option value is None, then the return value is too.
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  /**
   * e4.4
   * Write a function sequence that combines a list of Options into one Option containing
   * a list of all the Some values in the original list. If the original list contains None even
   * once, the result of the function should be None; otherwise the result should be Some
   * with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }

  def sequence01[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((elem, result) => result.flatMap(r => elem.map(e => e :: r)))
  }
}
