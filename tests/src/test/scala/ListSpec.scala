package dogs

import Predef._
import dogs.std.{intEq,intOrder}
import dogs.tests.arbitrary._
import scala.collection.Iterable
import scala.collection.immutable.{Nil,List=>SList,::}

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalatest.{FunSuite, PropSpec, Matchers}

//import List._

object ListTest extends Properties("ListTest") with ArbitraryList {
  // we need to provid our own tuple instance until
  // https://github.com/non/algebra/pull/82 is merged
  implicit def eqTuple2[A: Eq, B: Eq]: Eq[(A,B)] = new Eq[(A,B)] {
    def eqv(l: (A,B), r: (A,B)) = l._1 == r._1 && l._2 == r._2
  }
  // implicit val intBooleanArb: Arbitrary[Int => Boolean] = {
  //   val intGen = implicitly[Arbitrary[Int]].arbitrary
  //   Arbitrary(Gen.oneOf(
  //     Gen.const((_: Int) => true),
  //     Gen.const((_: Int) => false),
  //     Gen.choose(2, 5).map(n => (a: Int) => a % n == 0),
  //     Gen.choose(2, 5).map(n => (a: Int) => a % n != 0),
  //     intGen.map(n => (_: Int) > n),
  //     intGen.map(n => (_: Int) < n)
  //   ))
  // }

  implicit class IterableOps[A](as: Iterable[A]) {
    def toScalaList: List[A] = List.fromIterable(as)
  }

  property("map") =
    forAll { (xs: List[Int], n: Int) =>
      xs.toScalaList.map(_ ^ n) == xs.map(_ ^ n).toScalaList
    }

  property("filter") =
    forAll { (xs: List[Int], n: Int) =>
      xs.toScalaList.filter(_ < n) == xs.filter(_ < n).toScalaList
    }

//  property("flatMap") =
//    forAll { (xs: SList[Int], n: Int) =>
//      val f = (x: Int) => if ((x & 1) == 1) x :: x :: Nil else x :: Nil
//      xs.flatMap(x => f(x)) == xs.map(f).flatMap(x => f(x))
//    }

  property("isEmpty") =
    forAll { (xs: List[Int]) =>
      xs.toScalaList.isEmpty == xs.isEmpty
    }

  property("toNel") =
    forAll { (xs: SList[Int]) =>
      xs.headOption.map(_ => xs.toScalaList) == List.fromIterable(xs).toNel.toScalaOption
    }

  property("cons") =
    forAll { (xs: List[Int], x: Int) =>
      (x :: xs).toScalaList == x :: xs.toScalaList
    }

  property("concat") =
    forAll { (xs: List[Int], ys: List[Int]) =>
      (xs ::: ys).toScalaList == xs.toScalaList ::: ys.toScalaList
    }

//   property("coflatMap") =
//     forAll { (xs0: List[Int], x0: Int) =>
//       val xs: Nel[Int] = x0 :: xs0
//       def findMin(xs: Nel[Int]): Int = xs.reduceLeft(intOrder.min)
//       val ys = xs.coflatMap(ns => findMin(ns))
//       ys.head == (x0 :: xs0).min
//     }

  property("exists") =
    forAll { (xs: SList[Int], x: Int) =>
      xs.exists(_ > x) == xs.toScalaList.exists(_ > x)
      xs.exists(_ == x) == xs.toScalaList.exists(_ == x)
      xs.exists(_ != x) == xs.toScalaList.exists(_ != x)
    }

  property("forall") =
    forAll { (xs: SList[Int], x: Int) =>
      xs.forall(_ > x) == xs.toScalaList.forall(_ > x)
      xs.forall(_ == x) == xs.toScalaList.forall(_ == x)
      xs.forall(_ != x) == xs.toScalaList.forall(_ != x)
    }

//   property("find") =
//     forAll { (xs: List[Int], x: Int) =>
//       xs.find(_ > x) == xs.toScalaList.find(_ > x)
//       xs.find(_ == x) == xs.toScalaList.find(_ == x)
//       xs.find(_ != x) == xs.toScalaList.find(_ != x)
//     }

  property("contains") =
    forAll { (xs: SList[Int], x: Int) =>
      xs.contains(x) == xs.toScalaList.contains(x)
    }

  property("reverse") =
    forAll { (xs: SList[Int]) =>
      xs.reverse.toScalaList == xs.toScalaList.reverse
    }

  property("take/drop") =
    forAll { (xs: List[Int], n: Int) =>
      xs.take(n).toScalaList == xs.toScalaList.take(n)
      xs.drop(n).toScalaList == xs.toScalaList.drop(n)
    }

}
