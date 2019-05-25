package io.neal.chapter04

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }

  def flatMap01[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(v) => Some(v)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) => if (f(v)) Some(v) else None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
