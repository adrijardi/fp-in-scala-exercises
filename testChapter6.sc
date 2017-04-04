import Chapter6._

val (res1, rng) = nonNegativeInt(SimpleRNG(100))

val (res2, rng2) = double(rng)

val (res3, rng3) = intDouble(rng2)

val (res4, rng4) = doubleInt(rng3)

val (res5, rng5) = double3(rng4)

val (res6, rng6) = ints(6)(rng5)

val (res7, rng7) = double_(rng6)

val (res8, rng8) = map2(ints(6), double_)( (a,b) => a.map(_*b) )(rng7)

val (res9, rng9) = sequence(List(double_, double_, nonNegativeInt(_)))(rng8)

val (res10, rng10) = flatMap(double_)(d => ints((d*10).toInt)(_))(rng9)

double_(rng7)._1 * 3
val (res11, rng11) = map_(double_)( _ * 3)(rng7)

val (res12, rng12) = map2_(ints(6), double_)( (a,b) => a.map(_*b) )(rng7)

val inputs = List(Coin, Turn, Coin, Turn)
val initialState: Machine = Machine(true, 10, 2)

val ( (coins, candies), machine) = simulateMachine(inputs).run(initialState)
