import Chapter6._

val (res1, rng) = nonNegativeInt(SimpleRNG(100))

val (res2, rng2) = double(rng)

val (res3, rng3) = intDouble(rng2)

val (res4, rng4) = doubleInt(rng3)

val (res5, rng5) = double3(rng4)

val (res6, rng6) = ints(6)(rng5)

