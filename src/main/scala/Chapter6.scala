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

}
