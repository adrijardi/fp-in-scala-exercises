import scala.language.higherKinds

/**
  * Functional programming in Scala, exercises chapter 4
  */
object Chapter4 {

  /**
    * Exercise 4.1
    */
  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case _ => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case _: Some[A] => this
      case _ => ob
    }

    def filter(f: A => Boolean): Option[A] = flatMap { v =>
      if(f(v))
        Some(v)
      else
        None
    }
  }

  case class Some[A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def none[T]: Option[T] = None
  }

  /**
    * Exercise 4.2
    */
  def mean(xs: Seq[Double]): Option[Double] = if(xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { meanRes =>
    val varianceValues = xs.map(x => math.pow(x - meanRes, 2))
    mean(varianceValues)
  }

  /**
    * Exercise 4.3
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    a1 <- a
    b1 <- b
  } yield f(a1,b1)

  /**
    * Exercise 4.4
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    Some(
      a.foldRight[List[A]](Nil)( (a, res) => a match {
        case Some(v) => v :: res
        case None => return None
      })
    )

  /**
    * Exercise 4.5
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    Some(
      a.foldRight[List[B]](Nil)( (a, res) => f(a) match {
        case Some(v) => v :: res
        case None => return None
      })
    )
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)


  /**
    * Exercise 4.6
    */
  sealed trait Either[+E, +A] {

    def map[B](f: A => B): Either[E, B] = flatMap(r => Right(f(r)))

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => f(v)
      case l: Left[E] => l
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case r: Right[A] => r
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
      case (Right(a), Right(bv)) => Right(f(a,bv))
      case (a: Left[E], _) => a
      case (_, l: Left[EE]) => l
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def left[E, A](l: E): Either[E, A] = Left(l)
    def right[E, A](r: A): Either[E, A] = Right(r)

    /**
      * Exercise 4.7
      */
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      Right(
        as.foldRight[List[B]](Nil)( (v, res) => f(v) match {
          case Right(r) => r :: res
          case l : Left[E] => return l
        })
      )
  }

  /**
    * Exercise 4.8
    */
  case class NonEmptyList[A](head: A, tail: List[A]) {
    def ::(a: A) = NonEmptyList(a, head :: tail)
  }

  sealed trait Validation[+E, +A] {
    def map[B](f: A => B): Validation[E, B] = this match {
      case Success(v) => Success(f(v))
      case e @ Failure(_) => e
    }

  }

  case class Success[A](a: A) extends Validation[Nothing, A]
  case class Failure[E](nonEmptyList: NonEmptyList[E]) extends Validation[E, Nothing]

}
