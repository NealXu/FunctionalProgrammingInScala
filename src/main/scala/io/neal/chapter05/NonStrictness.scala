package io.neal.chapter05

object NonStrictness {

  def ifEx[A](condition: Boolean, onTrue: () => A, onFalse: () => A): A = {
    if (condition) onTrue() else onFalse()
  }

  def ifEx01[A](condition: Boolean, onTrue: => A, onFalse: => A): A = {
    if (condition) onTrue else onFalse
  }
}
