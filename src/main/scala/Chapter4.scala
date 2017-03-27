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
      a.foldLeft[List[A]](Nil)( (res, a) => a match {
        case Some(v) => v :: res
        case None => return None
      })
    )

  /**
    * Exercise 4.5
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

  }
}
