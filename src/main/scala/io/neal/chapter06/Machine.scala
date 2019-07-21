package io.neal.chapter06


/**
 * e6.11
 * Hard: To gain experience with the use of State, implement a finite state automaton
 * that models a simple candy dispenser. The machine has two types of input: you can
 * insert a coin, or you can turn the knob to dispense candy. It can be in one of two
 * states: locked or unlocked. It also tracks how many candies are left and how many
 * coins it contains.
 */
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  private var machine: Option[Machine] = None

  def apply(locked: Boolean, candies: Int, coins: Int): Machine = {
    machine.getOrElse {
      val m = new Machine(locked, candies, coins)
      machine = Some(m)
      m
    }
  }

}

