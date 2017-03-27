import Chapter2._
import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary._
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.Checkers._

class Chapter2Spec extends WordSpec with Matchers {

  val smallValue: Gen[Int] = Gen.choose(3, 40)

  "fib should fulfill the rules" in {
    check(Prop.forAll(smallValue) {
      (a: Int) =>
        fib(a) === fib(a-2) + fib(a-1)
    })
  }

  "isSorted works for sorted lists of numbers" in {
    val orderedFn = (a: Int, b: Int) => a <= b

    check((list: Array[Int]) => isSorted(list.sorted, orderedFn))
  }

  "isSorted works for unsorted lists of numbers" in {
    val orderedFn = (a: Int, b: Int) => a <= b
    val unorderedList = arbitrary[Array[Int]] suchThat(list =>
      list.length > 1 &&
      list.zip(list.tail).exists {
        case(a: Int,b: Int) => a > b
      }
    )

    check(Prop.forAll(unorderedList) { list =>
      ! isSorted(list, orderedFn)
    })
  }

  "curry works as expected" in {
    def func(a: Int, b: Int): String = s"$a-$b"

    curry(func)(6)(3) shouldBe "6-3"
  }

  "uncurry works as expected" in {
    def func(a: Int, b: Int): String = s"$a-$b"

    uncurry(curry(func))(6,3) shouldBe "6-3"
  }

  "compose works as expected" in {
    val f1: Int => Double = _ + 10
    val f2: Double => Double = math.sin


    compose(f2, f1)(3) shouldBe 0.4201670368266409
  }

}
