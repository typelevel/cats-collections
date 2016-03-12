package dogs
package tests

import Predef._
import syntax.birds._
import dogs.tests.arbitrary.all._
import cats._
import cats.std.int._
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests, CartesianTests}
import org.scalacheck._
import org.scalacheck.Prop.{forAll,secure}
import algebra.laws.{GroupLaws, OrderLaws}

class DListSpec extends DogsSuite {
  import DList._
  import List._
  import arbitrary.list._
  import arbitrary.dlist._

  checkAll("DList[Int]", GroupLaws[DList[Int]].monoid)
  checkAll("DList[Int]", OrderLaws[DList[Int]].eqv)

  checkAll("MonadCombine[DList]", MonadCombineTests[DList].monadCombine[Int,Int,Int])
  checkAll("MonadCombine[DList]", SerializableTests.serializable(MonadCombine[DList]))
  checkAll("Traverse[DList]", TraverseTests[DList].traverse[Int, Int, Int, DList[Int], Option, Option])
  checkAll("Traverse[DList]", SerializableTests.serializable(Traverse[DList]))

  test("sanity check")(forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.toList should matchTo(l)
  })

  test("headOption")(forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.headOption should be(l.headOption)
  })

  test("tailOption")(forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.tailOption.map(_.toList) should be (l.tailOption)
  })

  test("isEmpty")(forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.isEmpty should be(l.isEmpty)
  })

  test("foldr")(forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.foldr[List[Int]](Eval.now(List.empty))((l,a) => a.map(l :: _)).value should be (l)
  })

  test("flatMap")(forAll { (ls: List[List[Int]]) =>
    DList(ls.map(DList.apply)).flatMap(identity).toList should be (ls.flatMap(identity))
  })

  test("map")(forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.map(_ + 1).toList should be (l.map(_ + 1))
  })

  test("stack safe append"){
    val dl = List.fill(100000)(1).foldLeft[DList[Int]](DList.empty)((dl,i) =>
      dl ++ DList(List(i))
    )

    dl.headOption should be (Some(1))
  }

  test("stack safe post"){
    val dl = List.fill(100000)(1).foldLeft[DList[Int]](DList.empty)(_ :+ _)
    dl.headOption should be (Some(1))
  }

  test("stack safe pre") {
    val dl = List.fill(100000)(1).foldLeft[DList[Int]](DList.empty)((dl,a) => a +: dl)
    dl.headOption should be(Some(1))
  }
}
