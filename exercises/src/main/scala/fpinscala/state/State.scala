package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, next) = rng.nextInt
    val n = if (v == Int.MinValue) 0 else v
    (Math.abs(n), next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, next) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), next)
  }

  def doubleViaMap (rng: RNG): (Double, RNG) =
    map (nonNegativeInt)(a => a / (Int.MaxValue.toDouble + 1)) (rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, nextRng) = rng.nextInt
    val (d, nextNextRng) = double(nextRng)
    ((n,d), nextNextRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), nextRng) = intDouble(rng)
    ((d,i), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, n) = double(rng)
    val (d2, nn) = double (n)
    val (d3, nnn) = double (nn)
    ((d1, d2, d3), nnn)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (Nil, rng)
    else {
      val (v, next) = rng.nextInt
      val (remaining, f) = ints (count - 1)(next)
      (v :: remaining, f)
    }

  def ints2 (count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go (count: Int, rng: RNG, ans: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (ans, rng)
      else {
        val (v, next) = rng.nextInt
        go (count - 1, next, v :: ans)
      }
    go (count, rng, List())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, nextRng) = ra (rng)
      val (b, finalRng) = rb (nextRng)
      (f (a,b), finalRng)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    map(fs.foldLeft(unit(List[A]()))((b ,a) => map2(a,b)(_ :: _)))(_.reverse)

  def intsViaSequence (count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nextRng) = f (rng)
      g (a)(nextRng)
    }

  def nonNegativeLessThan (n: Int): Rand[Int] = flatMap(nonNegativeInt)(a => {
    val mod = a % n
    if (a + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  })

  def mapViaFlatMap[A,B] (s : Rand[A])(f : A => B): Rand [B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap [A,B,C] (ar: Rand[A], br: Rand[B])(f: (A,B) => C): Rand [C] =
    flatMap(ar)(a => map(br)(f(a,_)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = new State (s => {
    val (a, n) = run (s)
    (f (a), n)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = new State (s => {
    val (a, n) = run (s)
    val (b, nn) = sb.run (n)
    (f (a,b), nn)
  })
  def flatMap[B](f: A => State[S, B]): State[S, B] = new State (s => {
    val (a, n) = run (s)
    f (a).run(n)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State._
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map ((modify[Machine] _) compose update))
    s <- get
  } yield (s.coins, s.candies)
}

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    // Fix this - implement the business logic of the game
    def convert = (i : Input) => (m: Machine) => m

    val y = inputs.map (convert andThen State.modify[Machine])
    State.sequence(y).flatMap(f => State.get).map(m => (m.coins, m.candies))
  }

  def unit [A, S] (a: A): State[S, A] = new State (s => (a, s))

  def sequence [A, S] (s : List[State[S, A]]): State [S, List[A]] =
    s.foldRight (unit(List[A]()): State[S, List[A]]) ((a,b) => a.map2(b)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
