package dogs
package tests

import Predef._
import dogs.tests.arbitrary.all._
import cats._
import cats.implicits._
import scala.collection.Iterable
import scala.collection.immutable.{List=>SList}
import cats.laws.discipline.{TraverseTests, CoflatMapTests, ComonadTests, MonadTests, AlternativeTests, FoldableTests/*, ReducibleTests*/,SerializableTests, CartesianTests}
import cats.kernel.laws.{GroupLaws, OrderLaws}

class ListSpec extends DogsSuite {
  import List._
  import Option._

//  implicit val xx: Eq[Int] = implicitly[Eq[Int]]

//  implicit val yy: Eq[(Int,Int,Int)] = eqTuple3

//  implicit val zz: Eq[List[(Int,Int,Int)]] = List.listEq[(Int,Int,Int)]

  checkAll("Monoid[List[Int]]", GroupLaws[List[Int]].monoid)
  checkAll("List[Int]", OrderLaws[List[Int]].eqv)

  checkAll("List[Int]", CartesianTests[List].cartesian[Int, Int, Int])
  checkAll("Cartesian[List]", SerializableTests.serializable(Cartesian[List]))

  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", MonadTests[List].monad[Int, Int, Int])
  checkAll("Monad[List]", SerializableTests.serializable(Monad[List]))

  checkAll("List[Int]", AlternativeTests[List].alternative[Int, Int, Int])
  checkAll("Alternative[List]", SerializableTests.serializable(Alternative[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))


  checkAll("Nel[Int]", CartesianTests[Nel].cartesian[Int, Int, Int])
// HELP I don't know why this isn't workign:  checkAll("Cartesian[List]", SerializableTests.serializable(Cartesian[Nel]))
  checkAll("Nel[Int]", ComonadTests[Nel].comonad[Int, Int, Int])
  checkAll("ComonadMap[Nel]", SerializableTests.serializable(CoflatMap[Nel]))
  checkAll("Nel[Int]", FoldableTests[Nel].foldable[Int, Int])
  checkAll("Alternative[Nel]", SerializableTests.serializable(Foldable[Nel]))

// when we get a new cats release
//  checkAll("Nel[Int]", ReducibleTests[Nel].reducible[Int, Int])
//  checkAll("Alternative[Nel]", SerializableTests.serializable(Alternative[List]))

  checkAll("Nel[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[Nel]", SerializableTests.serializable(Traverse[List]))


  implicit class IterableOps[A](as: Iterable[A]) {
    def toScalaList: List[A] = List.fromIterable(as)
  }

  test("filter"){
    forAll { (xs: List[Int], n: Int) =>
      xs.toScalaList.filter(_ < n) should be (xs.filter(_ < n).toScalaList)
    }
  }

//  property("flatMap") =
//    forAll { (xs: SList[Int], n: Int) =>
//      val f = (x: Int) => if ((x & 1) == 1) x :: x :: Nil else x :: Nil
//      xs.flatMap(x => f(x)) == xs.map(f).flatMap(x => f(x))
//    }

  test("isEmpty")(
    forAll { (xs: List[Int]) =>
      xs.toScalaList.isEmpty should be (xs.isEmpty)
    })

  test("toNel")(
    forAll { (xs: SList[Int]) =>
      xs.headOption.map(_ => xs.toScalaList) should be(List.fromIterable(xs).toNel.toScalaOption)
    })

  test("exists")(
    forAll { (xs: SList[Int], x: Int) =>
      xs.exists(_ > x) should be(xs.toScalaList.exists(_ > x))
      xs.exists(_ == x) should be(xs.toScalaList.exists(_ == x))
      xs.exists(_ != x) should be(xs.toScalaList.exists(_ != x))
    })

  test("forall")(
    forAll { (xs: SList[Int], x: Int) =>
      xs.forall(_ > x) should be(xs.toScalaList.forall(_ > x))
      xs.forall(_ == x) should be(xs.toScalaList.forall(_ == x))
      xs.forall(_ != x) should be(xs.toScalaList.forall(_ != x))
    })

//   property("find") =
//     forAll { (xs: List[Int], x: Int) =>
//       xs.find(_ > x) == xs.toScalaList.find(_ > x)
//       xs.find(_ == x) == xs.toScalaList.find(_ == x)
//       xs.find(_ != x) == xs.toScalaList.find(_ != x)
//     }

  test("contains")(
    forAll { (xs: SList[Int], x: Int) =>
      xs.contains(x) should be(xs.toScalaList.contains(x))
    })

  test("reverse")(
    forAll { (xs: SList[Int]) =>
      xs.reverse.toScalaList should be(xs.toScalaList.reverse)
    })

  test("take/drop")(
    forAll { (xs: List[Int], n: Int) =>
      xs.take(n).toScalaList should be (xs.toScalaList.take(n))
      xs.drop(n).toScalaList should be (xs.toScalaList.drop(n))
    })

  @tailrec final def testSorted[A](l: List[A], last: Option[A])(implicit or: Order[A]): Boolean = l match {
    case El() => true
    case Nel(h,t) =>
      if(last.cata(or.gteqv(h, _), true))
        testSorted(t,some(h))
      else
        false
  }

  test("sorted")(
    forAll { (xs: List[Int]) =>
      testSorted(xs.sorted, none)
    }
  )

  test("sortBy")(
    forAll { (xs: List[String]) =>
      testSorted(xs.sortBy(_.length), none)
    }
  )

  test("min")(
    forAll { (xs: Nel[Int]) =>
      xs.min should be (xs.sorted1.head)
    }
  )

  test("minBy")(
    forAll { xs: Nel[String] =>
      xs.minBy(_.length).toScalaList should contain theSameElementsAs
        (xs.sortBy1(_.length).takeWhile(_.length == xs.sortBy1(_.length).head.length).toScalaList)
    }
  )

  test("max")(
    forAll { (xs: Nel[Int]) =>
      xs.max should be (xs.sorted1.last)
    }
  )

  test("maxBy")(
    forAll { (xs: Nel[String]) =>
      xs.maxBy(_.length).toScalaList should contain theSameElementsAs
        (xs.sortBy1(_.length).dropWhile(_.length != xs.sortBy1(_.length).last.length).toScalaList)
    }
  )

}
