import Chapter5._
import Stream._

def infinite(v: Int): Stream[Int] = cons(v, infinite(v*3))

def finite(v: Int): Stream[Int] =
  if(v < 10000){
    cons(v, finite(v*3))
  } else {
    Empty
  }

finite(3).toList

infinite(3).take(10).toList
infinite(3).drop(10).take(10).toList


