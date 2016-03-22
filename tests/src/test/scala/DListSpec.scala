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

class DListSpec extends SlowDogsSuite {
  import DList._
  import arbitrary.list._
  import arbitrary.dlist._

  checkAll("MonadCombine[DList]", MonadCombineTests[DList].monadCombine[Int,Int,Int])
  checkAll("MonadCombine[DList]", SerializableTests.serializable(MonadCombine[DList]))
  checkAll("Traverse[DList]", TraverseTests[DList].traverse[Int, Int, Int, DList[Int], Option, Option])
  checkAll("Traverse[DList]", SerializableTests.serializable(Traverse[DList]))

  test("sanity check")(forAll { (l1: List[Int], l2: List[Int]) =>
    val dl = fromLL(l1, l2)
    val l = l1 ++ l2

    dl.toList should matchTo(l)
  })

  test("headOption")(forAll { (l1: List[Int], l2: List[Int]) =>
    val dl = fromLL(l1, l2)
    val l = l1 ++ l2

    dl.headOption should be(l.headOption)
  })

  test("tailOption")(forAll { (l1: List[Int], l2: List[Int]) =>
    val dl = fromLL(l1, l2)
    val l = l1 ++ l2

    dl.tailOption.map(_.toList) should be (l.tailOption)
  })

  test("isEmpty")(forAll { (l1: List[Int], l2: List[Int]) =>
    val dl = fromLL(l1, l2)
    val l = l1 ++ l2

    dl.isEmpty should be(l.isEmpty)
  })

  test("foldRight")(forAll { (l1: List[Int], l2: List[Int]) =>
    val dl = fromLL(l1, l2)
    val l = l1 ++ l2

    dl.foldRight[List[Int]](Eval.now(List.empty))((l,a) => a.map(l :: _)).value should be (l)
  })

  test("map")(forAll { (l1: List[Int], l2: List[Int]) =>
    val dl = fromLL(l1, l2)
    val l = l1 ++ l2

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
