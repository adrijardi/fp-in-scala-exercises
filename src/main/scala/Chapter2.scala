import scala.annotation.tailrec

/**
  * Functional programming in Scala, exercises chapter 2
  */
object Chapter2 {

  /**
    * Exercise 2.1
    */
  def fib(n: Int): Int = {

    @tailrec
    def go(a: Int, b: Int, leftIter: Int): Int =
      if(leftIter == 0)
        b
      else
        go(b, a+b, leftIter-1)

    n match {
      case 1 => 0
      case 2 => 1
      case _ if n > 2 => go(0, 1, n-2)
      case _ => throw new Exception(s"fib can only be calculated for n > 0, value $n")
    }
  }

  /**
    * Exercise 2.2
    */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if(as.isEmpty)
      true
    else {
      for (i <- 0 until as.length -1) {
        if(! ordered(as(i), as(i+1)))
          return false
      }
      true
    }
  }

  /**
    * Exercise 2.3
    */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  /**
    * Exercise 2.4
    */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  /**
    * Exercise 2.5
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}
