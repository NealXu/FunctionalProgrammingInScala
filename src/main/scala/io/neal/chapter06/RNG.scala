package io.neal.chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
   * e6.1
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and
   * Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
   * Int.MinValue, which doesn’t have a non-negative counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    val ret = if (n == Int.MinValue) 0 else scala.math.abs(n)
    (ret, nextRng)
  }

  /**
   * e6.2
   * Write a function to generate a Double between 0 and 1, not including 1. Note: You can
   * use Int.MaxValue to obtain the maximum positive integer value, and you can use
   * x.toDouble to convert an x: Int to a Double.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRng)
  }

  /**
   * e6.3
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
   * (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
   * already written.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, rng1) = nonNegativeInt(rng)
    val (n2, rng2) = double(rng1)
    ((n1, n2), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n1, n2), newRng) = intDouble(rng)
    ((n2, n1), newRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (n1, rng1) = double(rng)
    val (n2, rng2) = double(rng1)
    val (n3, rng3) = double(rng2)
    ((n1, n2, n3), rng3)
  }

  /**
   * e6.4
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val (n1, rng1) = rng.nextInt
      val (l, rngTmp) = ints(count - 1)(rng1)
      (n1 :: l, rngTmp)
    }
  }

  def ints01(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(c: Int, r: RNG, tmp: List[Int]): (List[Int], RNG) = {
      if (c == count) {
        (tmp, r)
      } else {
        val (n, rngTmp) = r.nextInt
        loop(c + 1, rngTmp, n :: tmp)
      }
    }

    loop(0, rng, List[Int]())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng =>
      val (n, rngTmp) = s(rng)
      (f(n), rngTmp)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(x => x - x % 2)

  /**
   * e6.5
   * Use map to reimplement double in a more elegant way. See exercise 6.2.
   */
  def _double: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  /**
   * e6.6
   * Write the implementation of map2 based on the following signature. This function
   * takes two actions, ra and rb, and a function f for combining their results, and returns
   * a new action that combines them:
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (n1, rng1) = ra(rng)
      val (n2, rng2) = rb(rng1)
      (f(n1, n2), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, _double)
  val randDoubleInt: Rand[(Double, Int)] = both(_double, int)

  /**
   * e6.7
   * Hard: If you can combine two RNG transitions, you should be able to combine a whole
   * list of them. Implement sequence for combining a List of transitions into a single
   * transition. Use it to reimplement the ints function you wrote before. For the latter,
   * you can use the standard library function List.fill(n)(x) to make a list with x
   * repeated n times.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]())) {
      (x, y) =>
        rng =>
          val (n1, rng1) = y(rng)
          val (n2, rng2) = x(rng1)
          (n2 :: n1, rng2)
    }
  }

  def sequence01[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((a, z) => map2(a, z)((x, y) => x :: y))
  }

  def _ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  /**
   * e6.8
   * Implement flatMap, and then use it to implement nonNegativeLessThan.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (n1, rng1) = f(rng)
      g(n1)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      x =>
        val mod = x % n
        if (x + (n - 1) - mod > 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  /**
   * e6.9
   * Reimplement map and map2 in terms of flatMap. The fact that this is possible is what
   * we’re referring to when we say that flatMap is more powerful than map and map2.
   */

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(x => unit(f(x)))
  }


  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
  }

}
