import Chapter3._

Chapter3.init(List(1,0,0))


Chapter3.reverse(List(1,2,3))

Chapter3.foldLeft2(List(1,2,3), 0)(_+_)

Chapter3.foldRight2(List(1,2,3), 0)(_+_)

Chapter3.append(List(1,2,3), List(11,12,13))

Chapter3.add1(List(1,2,3))

stringify(List(1,2,3,4.0))

map(List(1,2,3))(_ * 3)

filter(List(1,2,3,4,5,6))(_ % 2 == 0)

flatMap(List(10,20,30))(e => List(e, e+1, e+2))

filter2(List(1,2,3,4,5,6))(_ % 2 == 0)

zipInt(List(1,2,3), List(4,5,6))

zipWith(List(1,2,3), List(4,5,6,7))(_+_)

hasSubsequence(List(1,2,3,4,5,6,7,8), List(2,3))
hasSubsequence(List(1,2,3,4,5,6,7,8), List(2,4))

val tree1 = Branch(Branch(Leaf(1), Leaf(4)), Leaf(10))

size(tree1)

maximum(tree1)

depth(tree1)

map(tree1)(_ * 2)

size2(tree1)

maximum2(tree1)

depth2(tree1)

map2(tree1)(_ * 2)

