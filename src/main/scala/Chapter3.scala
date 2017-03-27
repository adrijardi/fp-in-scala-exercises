import scala.annotation.tailrec

/**
  * Functional programming in Scala, exercises chapter 3
  */
object Chapter3 {

  /**
    * Exercise 3.2
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Cannot get tail of empty list")
    case _ :: xs => xs
  }

  /**
    * Exercise 3.3
    */
  def setHead[A](l: List[A], h: A): List[A] = h :: tail(l)

  /**
    * Exercise 3.4
    */
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, 0)       => l
    case (Nil, _)     => throw new Exception("Cannot drop elements from empty list")
    case (_ :: xs, _) => drop(xs, n-1)
  }

  def tailB[A](l: List[A]): List[A] = drop(l, 1)

  /**
    * Exercise 3.5
    */
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x :: xs if f(x) => dropWhile(xs, f)
    case _  => l
  }

  /**
    * Exercise 3.6
    */
  def init[A](l: List[A]): List[A] = {

    @tailrec
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case _ :: Nil => acc
      case x :: xs  => go(xs, x :: acc)
      case _        => acc
    }

    go(l, Nil).reverse
  }

  /**
    * Exercise 3.7
    */
  def product(ds: List[Double]): Double = ds.foldRight(1.0){
    case (0, _) => return 0.0
    case (a, b) => a * b
  }

  /**
    * Exercise 3.9
    */
  def length[A](as: List[A]): Int = as.foldRight(0)( (_, res) => res + 1)

  /**
    * Exercise 3.10
    */
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    def go(l: List[A], f: (B, A) => B, res: B): B = l match {
      case Nil => res
      case x :: xs => go(xs, f, f(res, x))
    }

    go(as, f, z)
  }

  /**
    * Exercise 3.11
    */
  def sum(l: List[Int]): Int = l.foldLeft(0)(_+_)

  def product2(l: List[Int]): Int = l.foldLeft(1)(_*_)

  def length2[A](l: List[A]): Int = l.foldLeft(0)((res, _) => res + 1)

  /**
    * Exercise 3.12
    */
  def reverse[A](l: List[A]): List[A] = l.foldLeft[List[A]](Nil)((res, e) => e :: res)

  /**
    * Exercise 3.13
    */
  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    val f1: (A, B) => B = (a,b) => f(b,a)
    as.foldRight(z)(f1)
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    val f1: (B, A) => B = (b,a) => f(a,b)
    as.foldLeft(z)(f1)
  }

  /**
    * Exercise 3.14
    */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1.foldRight(a2)( (e, res) => e :: res)

  /**
    * Exercise 3.15
    */
  def appendAll[A](l: List[List[A]]): List[A] = l.foldRight[List[A]](Nil)( (e, res) => append(e, res))

  /**
    * Exercise 3.16
    */
  def add1(l: List[Int]): List[Int] = l.foldRight(List.empty[Int])( (e, res) => (e+1) :: res)

  /**
    * Exercise 3.17
    */
  def stringify(list: List[Double]): List[String] = list.foldRight(List.empty[String])( (e, res) => e.toString :: res)

  /**
    * Exercise 3.18
    */
  def map[A,B](as: List[A])(f: A => B): List[B] = as.foldRight(List.empty[B])( (e, res) => f(e) :: res)

  /**
    * Exercise 3.19
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as.foldRight(List.empty[A])( (e, res) =>
    if(f(e)) {
      e :: res
    } else {
      res
    }
  )

  /**
    * Exercise 3.20
    */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as.foldRight(List.empty[B])( (e, res) => append(f(e), res))

  /**
    * Exercise 3.21
    */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as){ e =>
    if(f(e)) {
      List(e)
    } else {
      Nil
    }
  }

  /**
    * Exercise 3.22
    */
  def zipInt(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (ah :: at, bh :: bt) => ah + bh :: zipInt(at, bt)
  }

  /**
    * Exercise 3.23
    */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (ah :: at, bh :: bt) => f(ah, bh) :: zipWith(at, bt)(f)
  }

  /**
    * Exercise 3.24
    */
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case _ :: xs =>
      val found = !zipWith(sup, sub)((a, b) => a == b).contains(false)
      if(found)
        true
      else
        hasSubsequence(xs, sub)
  }



  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  /**
    * Exercise 3.25
    */
  def size[A](tree: Tree[A]): Int = tree match {
    case _: Leaf[A] => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /**
    * Exercise 3.26
    */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => math.max(maximum(l), maximum(r))
  }

  /**
    * Exercise 3.27
    */
  def depth[A](tree: Tree[A]): Int = tree match {
    case _: Leaf[A] => 1
    case Branch(l, r) => math.max(depth(l), depth(r)) + 1
  }

  /**
    * Exercise 3.28
    */
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
    * Exercise 3.29
    */
  def fold[A,B](tree: Tree[A])(fLeaf: Leaf[A] => B, fBranch: Branch[A] => B): B = tree match {
    case l: Leaf[A] => fLeaf(l)
    case b: Branch[A] => fBranch(b)
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1, b => size2(b.left) + size2(b.right) + 1)

  def maximum2(tree: Tree[Int]): Int = fold(tree)( l => l.value, b => math.max(maximum(b.left), maximum(b.right)))

  def depth2[A](tree: Tree[A]): Int = fold(tree)( _ => 1, b => math.max(depth(b.left), depth(b.right)) + 1)

  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(
    l => Leaf(f(l.value)),
    b => Branch(map(b.left)(f), map(b.right)(f))
  )

}
