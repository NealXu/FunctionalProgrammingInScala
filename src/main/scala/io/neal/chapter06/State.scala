package io.neal.chapter06

case class State[S, +A](run: S => (A, S)) {

  /**
   * e6.10
   * Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods
   * on the State case class where possible. Otherwise you should put them in a State
   * companion object.
   */
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State(s => (f(a), s)))
  }

  def map01[B](f: A => B): State[S, B] = {
    State { s =>
      val (a, s1) = run(s)
      (f(a), s1)
    }
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  def map2ex[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State { s =>
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }

}

object State {

  type Rand[A] = State[RNG, A]

  /**
   * e6.10
   * Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods
   * on the State case class where possible. Otherwise you should put them in a State
   * companion object.
   */
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

}
