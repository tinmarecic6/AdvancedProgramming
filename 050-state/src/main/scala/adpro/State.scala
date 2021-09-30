// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group: ____________
// AUTHOR1: __________
// AUTHOR2: __________
// AUTHOR3: __________

package adpro

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG (seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {

      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = SimpleRNG (newSeed)

      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt

      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)

    }

  }

  // Exercise 1 (CB 6.1)

  def nonNegativeInt (rng: RNG): (Int, RNG) = ???

  // Exercise 2 (CB 6.2)

  def double (rng: RNG): (Double, RNG) = ???

  // Exercise 3 (CB 6.3)

  def intDouble (rng: RNG) = ???

  def doubleInt (rng: RNG) = ???

  def boolean (rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i % 2 == 0, rng2) }

  // Exercise 4 (CB 6.4)

  def ints (count: Int) (rng: RNG) = ???

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A] (a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B] (s: Rand[A]) (f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s (rng)
      (f (a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map (nonNegativeInt) (i => i - i % 2)

  // Exercise 5 (CB 6.5) (Lazy is added so that the class does not fail
  // at load-time without your implementation).

  lazy val _double: Rand[Double] = ???

  // Exercise 6 (CB 6.6)

  def map2[A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = ???

  // this is given in the book

  def both[A,B] (ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2 (ra, rb) ((_, _))

  lazy val randIntDouble: Rand[(Int, Double)] = both (int, double)

  lazy val randDoubleInt: Rand[(Double, Int)] = both (double, int)

  // Exercise 7 (6.7)

  def sequence[A] (fs: List[Rand[A]]): Rand[List[A]] = ???

  def _ints (count: Int): Rand[List[Int]] = ???

  // Exercise 8 (6.8)

  def flatMap[A,B] (f: Rand[A]) (g: A => Rand[B]): Rand[B] = ???

  def nonNegativeLessThan (n: Int): Rand[Int] = ???

}

import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

  def map[B] (f: A => B): State[S, B] = ???

  def map2[B,C] (sb: State[S, B]) (f: (A, B) => C): State[S, C] = ???

  def flatMap[B] (f: A => State[S, B]): State[S, B] = ???

}

object State {

  import adpro.Stream

  type Rand[A] = State[RNG, A]

  def unit[S, A] (a: A): State[S, A] =
    State (s => (a, s))

  // Exercise 9 (6.10) continued

  def sequence[S,A] (sas: List[State[S, A]]): State[S, List[A]] = ???

  // This is given in the book:

  def modify[S] (f: S => S): State[S, Unit] = for {
    // Get the current state and assigns it to `s`.
     s <- get
     // Set the new state to `f` applied to `s`.
     _ <- set (f (s))
  } yield ()

  def get[S]: State[S, S] = State (s => (s, s))

  def set[S] (s: S): State[S, Unit] = State (_ => ((), s))

  def random_int: Rand[Int] =  State (_.nextInt)

  // Exercise 10

  def state2stream[S,A] (s: State[S,A]) (seed: S): Stream[A] = ???

  // Exercise 11 (lazy is added so that the class does not crash at load time
  // before you provide an implementation).

  lazy val random_integers = ???

}
