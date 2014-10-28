package fpinscala.ch6.second


case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  // 6.1
  // Write a function that uses RNG.nextInt to generate a random integer between 0 and
  // Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
  // Int.MinValue, which doesn't have a non-negative counterpart.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    (if (n < 0) -(n + 1) else n, rng2)
  }

  // 6.2
  // Write a function to generate a Double between 0 and 1, not including 1. Note: You can
  // use Int.MaxValue to obtain the maximum positive integer value, and you can use
  // x.toDouble to convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // 6.3
  // Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
  // already written.
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4
  // Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case n if n <= 0 => (List.empty[Int], rng)
    case n =>
      val (i, r) = rng.nextInt
      val (l, rr) = ints(n - 1)(r)
      (i :: l, rr)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }


  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // 6.5
  // Use map to reimplement double in a more elegant way. See exercise 6.2.
  def double_map(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // 6.6
  // Write the implementation of map2 based on the following signature. This function
  // takes two actions, ra and rb, and a function f for combining their results, and returns
  // a new action that combines them:
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // 6.7
  // Hard: If you can combine two RNG transitions, you should be able to combine a whole
  // list of them. Implement sequence for combining a List of transitions into a single
  // transition. Use it to reimplement the ints function you wrote before. For the latter,
  // you can use the standard library function List.fill(n)(x) to make a list with x
  // repeated n times.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((rand, acc) => map2(rand, acc)(_ :: _))

  def ints_sequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // 6.8
  // Implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  // 6.9
  // Reimplement map and map2 in terms of flatMap. The fact that this is possible is what
  // we’re referring to when we say that flatMap is more powerful than map and map2.
  def map_flatmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  // 6.9
  def map2_flatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))


  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

case class State[S, +A](run: S => (A, S)) {
  // 6.10
  def map[B](f: A => B): State[S, B] =
    flatMap(s => State.unit(f(s)))

  // 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  )

  // 6.10
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

}


object State {
  // 6.10
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  // 6.10
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}

// EXERCISE 6.11
// Hard: To gain experience with the use of State, implement a finite state automaton
// that models a simple candy dispenser. The machine has two types of input: you can
// insert a coin, or you can turn the knob to dispense candy. It can be in one of two
// states: locked or unlocked. It also tracks how many candies are left and how many
// coins it contains.
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val k = sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    })))
  for {
    _ <- k
    s <- get
  } yield (s.coins, s.candies)
}

}

