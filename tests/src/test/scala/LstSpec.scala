package dogs
package tests

import scala.Int
import scala.collection.Iterable
import scala.collection.immutable.{Nil,List,::}
import scala.Predef._

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalatest.{FunSuite, PropSpec, Matchers}

import cats._
import cats.std.all._
import cats.syntax.all._

//import Lst._

object LstTest extends Properties("LstTest") with ArbitraryLst {

  // we need to provid our own tuple instance until
  // https://github.com/non/algebra/pull/82 is merged
  implicit def eqTuple2[A: Eq, B: Eq]: Eq[(A,B)] =
    Eq.instance((l,r) => l._1 === r._1 && l._2 === r._2)

  implicit class IterableOps[A](as: Iterable[A]) {
    def toLst: Lst[A] = Lst.fromIterable(as)
  }

  property("map") =
    forAll { (xs: List[Int], n: Int) =>
      xs.toLst.map(_ ^ n) === xs.map(_ ^ n).toLst
    }

  property("filter") =
    forAll { (xs: List[Int], n: Int) =>
      xs.toLst.filter(_ < n) === xs.filter(_ < n).toLst
    }

  property("flatMap") =
    forAll { (xs: List[Int], n: Int) =>
      val f = (x: Int) => if ((x & 1) == 1) x :: x :: Nil else x :: Nil
      xs.toLst.flatMap(x => f(x).toLst) === xs.flatMap(f).toLst
    }

  property("isEmpty") =
    forAll { (xs: List[Int]) =>
      xs.toLst.isEmpty === xs.isEmpty
    }

  property("toNel") =
    forAll { (xs: List[Int]) =>
      xs.headOption.map(_ => xs.toLst) === xs.toLst.toNel
    }

  property("cons") =
    forAll { (xs: List[Int], x: Int) =>
      (x :: xs).toLst === x :: xs.toLst
    }

  property("concat") =
    forAll { (xs: List[Int], ys: List[Int]) =>
      (xs ::: ys).toLst === xs.toLst ::: ys.toLst
    }

  property("coflatMap") =
    forAll { (xs0: List[Int], x0: Int) =>
      val xs: Nel[Int] = x0 :: xs0.toLst
      def findMin(xs: Nel[Int]): Int = xs.reduceLeft(_ min _)
      val ys = xs.coflatMap(ns => findMin(ns))
      ys.head === (x0 :: xs0).min
    }

  property("exists") =
    forAll { (xs: List[Int], x: Int) =>
      xs.exists(_ > x) === xs.toLst.exists(_ > x)
      xs.exists(_ == x) === xs.toLst.exists(_ == x)
      xs.exists(_ != x) === xs.toLst.exists(_ != x)
    }

  property("forall") =
    forAll { (xs: List[Int], x: Int) =>
      xs.forall(_ > x) === xs.toLst.forall(_ > x)
      xs.forall(_ == x) === xs.toLst.forall(_ == x)
      xs.forall(_ != x) === xs.toLst.forall(_ != x)
    }

  property("find") =
    forAll { (xs: List[Int], x: Int) =>
      xs.find(_ > x) === xs.toLst.find(_ > x)
      xs.find(_ == x) === xs.toLst.find(_ == x)
      xs.find(_ != x) === xs.toLst.find(_ != x)
    }

  property("contains") =
    forAll { (xs: List[Int], x: Int) =>
      xs.contains(x) === xs.toLst.contains(x)
    }

  property("reverse") =
    forAll { (xs: List[Int]) =>
      xs.reverse.toLst === xs.toLst.reverse
    }

  property("take/drop") =
    forAll { (xs: List[Int], n: Int) =>
      xs.take(n).toLst === xs.toLst.take(n)
      xs.drop(n).toLst === xs.toLst.drop(n)
    }

}
