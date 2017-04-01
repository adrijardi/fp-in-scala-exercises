import Chapter5.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

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
    def take(n: Int): Stream[A] =
      if(n <= 0) {
        Empty
      } else {
        this match {
          case Empty => Empty
          case Cons(h, t) => Cons(h, () => t().take(n-1))
        }
      }

    def drop(n: Int): Stream[A] =
      if(n <= 0) {
        this
      } else {
        this match {
          case Empty => Empty
          case Cons(_, t) => t().drop(n-1)
        }
      }

    /**
      * Exercise 5.3
      */
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) =>
        if(p(h())) {
          Cons(h, () => t().takeWhile(p))
        } else {
          Empty
        }
    }

    /**
      * Exercise 5.4
      */
    @tailrec
    final def forAll(p: A => Boolean): Boolean = this match {
      case Empty => true
      case Cons(h, t) => p(h()) && t().forAll(p)
    }

    /**
      * Exercise 5.5
      */
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((e, r) =>
      if(p(e)) {
        cons(e, r)
      } else {
        Empty
      }
    )

    /**
      * Exercise 5.6
      */
    def headOption: Option[A] = foldRight[Option[A]](None)( (e, _) => return Some(e) )

    /**
      * Exercise 5.7
      */
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((e, r) => cons(f(e), r) )

    def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((e, r) =>
      if(f(e)) {
        cons(e, r)
      } else {
        r
      }
    )

    def append[AA >: A](a: Stream[AA]): Stream[AA] = foldRight(a)( (e, r) => cons(e, r) )

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((e, r) => f(e).append(r))

    /**
      * Exercise 5.13
      */
    def map2[B](f: A => B): Stream[B] = unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

    def take2(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, t), n1) if n1 > 0 => Some((h(), (t(), n1-1)))
      case _ => None
    }

    def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

    def zipWith[B,C](b: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, b)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some(( f(h1(), h2()), (t1(), t2()) ))
      case _ => None
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some( (Some(h1()), Some(h2())), (t1(), t2()) )
      case (Cons(h1,t1), _) => Some( (Some(h1()), None), (t1(), Empty) )
      case (_, Cons(h2,t2)) => Some( (None, Some(h2())), (Empty, t2()))
      case _ => None
    }

    /**
      * Exercise 5.14
      */
    def startsWith[AA >: A](s2: Stream[AA]): Boolean = zipAll(s2).forAll {
      case (_, None) => return true
      case (Some(a), Some(b)) => a == b
      case (None, _) => false
    }

    /**
      * Exercise 5.15
      */
    def tails: Stream[Stream[A]] = cons(this, unfold(this) {
      case Empty => None
      case Cons(_, t) => Some( (t(), t()) )
    })

    /**
      * Exercise 5.16
      */
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(z)(f))
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

    /**
      * Exercise 5.8
      */
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    /**
      * Exercise 5.9
      */
    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    /**
      * Exercise 5.10
      */
    def fibs: Stream[Long] = {
      def go(a: Long, b: Long): Stream[Long] = {
        cons(a+b, go(b, a+b))
      }

      cons(0, cons(1, go(0, 1)))
    }

    /**
      * Exercise 5.11
      */
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

    /**
      * Exercise 5.12
      */
    def fibs2: Stream[Long] = cons(0, cons(1L, unfold((0L,1L)){ case (a,b) => Some((a+b, (b, a+b))) } ))

    def from2(n: Int): Stream[Int] = unfold(n)( v => Some(v, v+1) )

    def constant2[A](a: A): Stream[A] = unfold(())(_ => Some(a, ()))

    def ones2: Stream[Int] = unfold(())(_ => Some(1, ()))

  }

}
