/**
  * Functional programming in Scala, exercises chapter 5
  */
object Chapter5 {

  sealed trait Stream[+A] {

    /**
      * Exercise 5.1
      */
    def toList: List[A] = {
      def doRec(s: Stream[A], r: List[A]): List[A] = s match {
        case Empty => r
        case Cons(h, t)=> doRec(t(), h() :: r)
      }

      doRec(this, Nil).reverse
    }

    /**
      * Exercise 5.2
      */
    def take(n: Int): Stream[A] = {
      if(n <= 0) {
        Empty
      } else {
        this match {
          case Empty => Empty
          case Cons(h, t) => Cons(h, () => t().take(n-1))
        }
      }
    }

    def drop(n: Int): Stream[A] = {
      if(n <= 0) {
        this
      } else {
        this match {
          case Empty => Empty
          case Cons(h, t) => t().drop(n-1)
        }
      }
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  }

}
