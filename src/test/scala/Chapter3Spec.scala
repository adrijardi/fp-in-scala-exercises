import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary._
import org.scalatest.prop.Checkers._
import org.scalatest.{Matchers, WordSpec}
import Chapter3._
import org.scalactic.Equality

import scala.math.{abs, max}

class Chapter3Spec extends WordSpec with Matchers {

  val nonEmpty: Gen[List[Char]] = Gen.nonEmptyListOf(Gen.alphaChar)

  "tail should get the tail of a non empty list" in {
    check(Prop.forAll(nonEmpty) { list =>
      tail(list) === list.tail
    })
  }

  "tail should fail on empty list" in {
    assertThrows[Exception](tail(Nil))
  }

  "setHead should get the tail of a non empty list" in {
    check(Prop.forAll(nonEmpty, arbitrary[Char]) { (list, c) =>
      setHead(list, c) === c :: list.tail
    })
  }

  "setHead should fail on empty list" in {
    assertThrows[Exception](setHead(Nil, 'a'))
  }

  "drop should work when dropping an number less or equal than the list size" in {
    val gen = for {
      l <- arbitrary[List[Char]]
      n <- Gen.choose(0, 100) if n <= l.length
    } yield (l, n)
    check(Prop.forAll(gen) { case (list, numDrop) =>
      drop(list, numDrop).length === list.length - numDrop
    })
  }

  "drop should fail when dropping an number greater than the list size" in {
    val gen = for {
      l <- arbitrary[List[Char]]
      n <- Gen.choose(0, 100) if n > l.length
    } yield (l, n)
    check(Prop.forAll(gen) { case (list, numDrop) =>
      Prop.throws(classOf[Exception])(drop(list, numDrop))
    })
  }

  "dropWhile should function properly" in {
    check(Prop.forAll(arbitrary[List[Boolean]]) { list =>
      val res = dropWhile(list, (b: Boolean) => !b )
      res.headOption.getOrElse(true) === true &&
      res.count(identity) === list.count(identity)
    })
  }

  "init should drop the last element" in {
    check(Prop.forAll(arbitrary[List[Int]]) { list =>
      init(list) === list.take(list.length-1)
    })
  }

  "product should multiply all elements" in {
    implicit object DoubleEqualityTolerance extends Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean = b match {
        case r: Double =>
          val tolerance = abs(max(a,r)) * 1E-15
          abs(a - r) < tolerance
        case _ => false
      }
    }

    check(Prop.forAll(Gen.listOf(Gen.choose(-100.0, 100.0))) { list =>
      product(list) === list.fold(1.0)(_*_)
    })
  }

  "length gets the right length for the list" in {
    check(Prop.forAll(Gen.choose(0, 10000)) { actualLenght =>
      Prop.forAll(Gen.listOfN(actualLenght, Gen.alphaChar)) { list =>
        Chapter3.length(list) === actualLenght
      }
    })
  }

  "foldleft applies the function correctly for sum" in {
    val f: (Int, Int) => Int = _ + _
    check(Prop.forAll(Gen.listOfN(100, Gen.choose(-100, 100))) { list =>
      foldLeft(list, 0)(f) === list.sum
    })
  }

  "reverse should do as it says :)" in {
    check(Prop.forAll(arbitrary[List[Char]]) { list =>
      reverse(list) === list.reverse
    })
  }

  "append should put 2 lists together" in {
    check(Prop.forAll(arbitrary[(List[Char], List[Char])]) { case (list1, list2) =>
      append(list1, list2) === list1 ::: list2
    })
  }

  "appendAll should append all lists together" in {
    val a = List(1,2,3,4)
    val b = List(15,14,13,12,11)
    val c = List(99,1,66)

    appendAll(List(a,b,c)) shouldBe List(1,2,3,4,15,14,13,12,11,99,1,66)
  }
}
