package io.neal.chapter04

sealed trait Either[+E, +A] {

  /**
   * e4.6
   * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the
   * Right value.
   */
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) =>
        b match {
          case Left(ee) => Left(ee)
          case Right(aa) => Right(f(a, aa))
        }
    }
  }

  def map2Ex[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {a <- this; b1 <- b} yield f(a, b1)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, z) => for {m <- f(x); n <- z} yield m :: n)
  }

}
