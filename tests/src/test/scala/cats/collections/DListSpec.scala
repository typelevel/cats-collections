package cats.collections
package tests

import cats.collections.tests.arbitrary._
import cats.collections.tests.arbitrary.cogen._
import cats._
import cats.implicits._
import cats.kernel.laws.discipline.{SerializableTests => _, _}
import cats.laws.discipline._
import org.scalacheck._
import Cogen._
import catalysts.Platform
import cats.tests.CatsSuite

class DListSpec extends CatsSuite with ArbitraryDList {
  import DList._

  checkAll("DList[Int]", MonoidTests[DList[Int]].monoid)
  checkAll("DList[Int]", EqTests[DList[Int]].eqv)

  checkAll("Traverse[DList]", TraverseTests[DList].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[DList]", SerializableTests.serializable(Traverse[DList]))

  test("headOption")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.headOption should be(l.headOption)
  })

  test("tailOption")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.tailOption.map(_.toList) should be (l.headOption.map(_ => l.tail))
  })

  test("isEmpty")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.isEmpty should be(l.isEmpty)
  })

  test("foldRight")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.foldRight[List[Int]](Eval.now(List.empty))((l,a) => a.map(l :: _)).value should be (l)
  })

  test("map")(forAll {(l: List[Int]) =>
    val dl = DList(l)
    dl.map(_ + 1).toList should be (l.map(_ + 1))
  })

  test("stack safe append"){
    val fill = if(Platform.isJvm) 100000 else 1000
    val dl = List.fill(fill)(1).foldLeft[DList[Int]](DList.empty)((dl,i) =>
      dl ++ DList(List(i))
    )

    dl.headOption should be (Some(1))
  }
  test("stack safe post"){
    val fill = if(Platform.isJvm) 100000 else 1000
    val dl = List.fill(fill)(1).foldLeft[DList[Int]](DList.empty)(_ :+ _)
    dl.headOption should be (Some(1))
  }


  test("stack safe pre") {
    val fill = if(Platform.isJvm) 100000 else 1000
    val dl = List.fill(fill)(1).foldLeft[DList[Int]](DList.empty)((dl,a) => a +: dl)
    dl.headOption should be(Some(1))
  }

}
