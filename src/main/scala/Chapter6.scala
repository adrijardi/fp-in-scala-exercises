import Chapter6.State.{get, modify, set}

/**
  * Functional programming in Scala, exercises chapter 6
  */
object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
    * Exercise 6.1
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, nextRng) = rng.nextInt
    val nonNegative = math.abs(value)
    if(nonNegative > 0)
      (nonNegative, nextRng)
    else
      nonNegativeInt(nextRng)
  }

  /**
    * Exercise 6.2
    */
  def double(rng: RNG): (Double, RNG) = {
    val (value, nextRng) = nonNegativeInt(rng)
    val capped = math.min(value, Int.MaxValue-1)
    (capped.toDouble / Int.MaxValue.toDouble, nextRng)
  }

  /**
    * Exercise 6.3
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, nextRng) = rng.nextInt
    val (doubleV, endRng) = double(nextRng)
    ( (int, doubleV), endRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val intD = intDouble(rng)
    (intD._1.swap, intD._2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ( (d1,d2,d3), r3)
  }

  /**
    * Exercise 6.4
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, r: List[Int], rng: RNG): (List[Int], RNG) =
      if(c <= 0)
        (r, rng)
      else {
        val (int, nextRng) = rng.nextInt
        go(c-1, int :: r, nextRng)
      }

    go(count, Nil, rng)
  }

  /**
    * Exercise 6.5
    */
  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def double_ : Rand[Double] =
    map[Int, Double](nonNegativeInt) { rnd =>
      math.min(rnd, Int.MaxValue - 1).toDouble / Int.MaxValue
    }

  /**
    * Exercise 6.6
    */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, newNrg) = ra(rng)
    val (b, endNrg) = rb(newNrg)
    ( f(a,b), endNrg)
  }

  /**
    * Exercise 6.7
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
    case Nil => (Nil, rng)
    case h :: t =>
      val (l, newNrg) = h(rng)
      val r: (List[A], RNG) = sequence(t)(newNrg)
      (l :: r._1, r._2)
  }

  /**
    * Exercise 6.8
    */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, newRng) = f(rng)
    g(a)(newRng)
  }

  /**
    * Exercise 6.9
    */
  def map_[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    val lift: A => Rand[B] = a => rng => (f(a), rng)
    flatMap(s)(lift)
  }

  def map2_[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)( a => map(rb) (b => f(a, b)))

  /**
    * Exercise 6.10
    */
  case class State[S,+A](run: S => (A,S)) {
    def map[B](f: A => B): State[S,B] = flatMap(f.andThen(State.unit))

    def flatMap[B](f: A => State[S,B]): State[S,B] = State{
      s: S => {
        val (a, s1) = run(s)
        f(a).run(s1)
      }
    }

  }

  object State {
    def unit[S,A](a: A): State[S,A] = State(s => (a, s))

    def map2[S,A,B,C](sa: State[S,A], sb: State[S,B])(f: (A, B) => C): State[S,C] =
      sa.flatMap(a => sb.map( f(a,_) ))

    def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = State { si: S =>

      fs.foldLeft( (List.empty[A], si) )( (res, e) => {
        val (a, newS): (A, S) = e.run(res._2)
        (a :: res._1, newS)
      })
    }

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  /**
    * Exercise 6.11
    */
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  val insertCoin: State[Machine, Unit] = modify {
    case Machine(true, candies, coins) if candies > 0 => Machine(false, candies, coins +1)
    case s => s
  }

  val turnKnob: State[Machine, Unit] = modify {
    case Machine(false, candies, coins) => Machine(true, candies -1, coins)
    case o => o
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    val modifications: List[State[Machine, Unit]] = inputs.map {
      case Coin => insertCoin
      case Turn => turnKnob
    }

    for {
      _ <- State.sequence(modifications)
      s <- get
    } yield (s.coins, s.candies)
  }

}
