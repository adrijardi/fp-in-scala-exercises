import Chapter5._
import Stream._

def gen[A](s: A, f: (A) => A, doWhile: A => Boolean): Stream[A] =
  if(doWhile(s)){
    cons(s, gen(f(s), f, doWhile))
  } else {
    Empty
  }

def infinite(s: Int, f: (Int) => Int): Stream[Int] = gen[Int](s, f, _ => true)
val infinite3x3 = infinite(3, _ * 3)

def finite(s: Int, f: (Int) => Int, max: Int = 10000): Stream[Int] = gen[Int](s, f, _ < max)


finite(3, _ * 3).toList

infinite3x3.take(10).toList
infinite3x3.drop(10).take(10).toList


infinite3x3.takeWhile(_ < 10000).toList

infinite3x3.forAll(_ < 100000)

infinite3x3.takeWhile2(_ < 10000).toList

infinite3x3.headOption

infinite(1, _+1).map(_+1).take(10).toList
finite(1, _ + 1).map(_+1).toList

finite(1, _ + 1).filter(_ % 2 == 0).toList

gen[Int](1, _+1, _ < 10).append(Stream.cons(2000, Empty)).toList

gen[Int](1, _+1, _ < 10).flatMap(v => Stream.cons(v, Stream.cons(v+10, Empty))).toList

constant(6).take(100).toList
from(6).take(100).toList

fibs.take(100).toList

fibs2.take(100).toList
constant2(6).take(100).toList
from2(6).take(100).toList
ones2.take(100).toList


infinite(1, _+1).map2(_+1).take(10).toList
infinite3x3.take2(10).toList
infinite3x3.takeWhile3(_ < 10000).toList
infinite3x3.zipWith(finite(1, _ + 2, 10))(_+_).toList
finite(1, _ + 2, 20).zipAll(finite(1, _ + 1, 20)).toList

infinite3x3.startsWith(infinite3x3.take(5))
infinite3x3.startsWith(Empty)
infinite3x3.startsWith(Stream.constant(1))
finite(1,_+1,10).startsWith(finite(1,_+1,10))
finite(1,_+1,10).startsWith(finite(1,_+1,11))

finite(1,_+1,10).tails.map(_.toList).toList

Stream(1,2,3).scanRight(0)(_ + _).toList

