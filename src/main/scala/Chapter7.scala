import java.util.concurrent.ExecutorService

/**
  * Functional programming in Scala, exercises chapter 7
  */
object Chapter7 {

  sealed trait Par[+A]
  case class SyncPar[+A](a: A) extends Par[A]
  case class AsyncPar[+A](a: () => A) extends Par[A]

  /**
    * Exercise 7.2
    */
  //def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  /**
    * Exercise 7.2
    */
  object Par {
    val es: ExecutorService

    def unit[A](a: A): Par[A] = SyncPar(a)

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = lazyUnit(f(run(a), run(b)))

    def fork[A](a: => Par[A]): Par[A] = AsyncPar(() => a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](a: Par[A]): A = a match {
      case SyncPar(v) => v
      case AsyncPar(v) =>
        es.submit(v)
    }

  }

}
