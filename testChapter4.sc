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

