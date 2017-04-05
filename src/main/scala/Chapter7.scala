/**
  * Functional programming in Scala, exercises chapter 7
  */
object Chapter7 {

  case class Par[+A](a: () => A)

  /**
    * Exercise 7.2
    */
  //def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  /**
    * Exercise 7.2
    */
  object Par {
    def unit[A](a: A): Par[A] = ???
    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???
    def fork[A](a: => Par[A]): Par[A] = ???
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A](a: Par[A]): A = ???
  }

}
