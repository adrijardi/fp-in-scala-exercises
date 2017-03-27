import Chapter4._

Some(3).map(_+1)
Option.none[Int].map(_+1)

Some(3).flatMap(v => Some(v+4))
Some(3).flatMap(_ => None)
None.flatMap(_ => Some(1))

Some(3).getOrElse(4)
None.getOrElse(4)

Some(3).orElse(Some(4))
None.orElse(Some(4))
None.orElse(None)

Some(3).filter(_ == 2)
Some(2).filter(_ == 2)
None.filter(_ == 2)

mean(List(1, 10, 10))
mean(Nil)

variance(List(1, 10, 10))
variance(Nil)

map2(Some(1), Some(2))(_+_)
map2(Some(1), Option.none[Int])(_+_)
map2(Option.none[Int], Option.none[Int])(_+_)

sequence(Nil)
sequence(List(Some(1), Some(2)))
sequence(List(Some(1), Some(2), None))
sequence(List(None, Some(1), Some(2)))

traverse(Nil)(identity)
traverse(List(1,2,3))(Some(_))
traverse(List(1,2,3))(v => if(v%2 == 0) None else Some(v))
traverse(List(1,2,3))(v => Some(v*3))

sequence2(Nil)
sequence2(List(Some(1), Some(2)))
sequence2(List(Some(1), Some(2), None))
sequence2(List(None, Some(1), Some(2)))

import Either.left
Right(4).map(_+1)
left[String, Int]("Error or something").map(_+1)
left[String, Int]("Error or something").orElse(Right("Hello!")).orElse(Right("Bye :("))
Right(41).map2(Right(1))(_+_)
left[String, Int]("Error or something").map2(Right(1))(_+_)

Either.traverse(Nil)(identity)
Either.traverse(List(1,2,3))(Right(_))
Either.traverse(List(1,2,3))(v => if(v%2 == 0) Left("Yay, left") else Right(v))
Either.traverse(List(1,2,3))(v => Right(v*3))