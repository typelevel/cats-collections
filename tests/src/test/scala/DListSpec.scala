package dogs
package tests

import Predef._
import bedazzle.birds._
import dogs.std.intEq
import dogs.tests.arbitrary._

import org.scalacheck._
import org.scalacheck.Prop.{forAll,secure}

object DListSpec extends Properties("DList") with ArbitraryList {
  import List._

  def fromLL(ls: List[List[Int]]): DList[Int] = 
    ls.foldLeft(DList.empty[Int])((dl,l) => dl ++ DList(l))

  property("sanity check") = forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    Eq[List[Int]].eqv(dl.toList, l)
  }

  property("headOption") = forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.headOption == l.headOption
  }

  property("tailOption") = forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.tailOption.map(_.toList) == l.tailOption
  }

  property("isEmpty") = forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.isEmpty == l.isEmpty
  }

  property("foldr") = forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.foldr[List[Int]](Eval.now(List.empty))((l,a) => a.map(l :: _)).value == l
  }

  property("flatMap") = forAll { (ls: List[List[Int]]) =>
    DList(ls.map(DList.apply)).flatMap(identity).toList == ls.flatMap(identity)
  }

  property("map") = forAll { (ls: List[List[Int]]) =>
    val dl = fromLL(ls)
    val l = ls.flatMap(identity)

    dl.map(_ + 1).toList == l.map(_ + 1)
  }

  property("stack safe append") = secure {
    val dl = List.fill(100000)(1).foldLeft[DList[Int]](DList.empty)((dl,i) =>
      dl ++ DList(List(i))
    )

    dl.headOption == Some(1)
  }

  property("stack safe post") = secure {
    val dl = List.fill(100000)(1).foldLeft[DList[Int]](DList.empty)(_ :+ _)
    dl.headOption == Some(1)
  }

  property("stack safe pre") = secure {
    val dl = List.fill(100000)(1).foldLeft[DList[Int]](DList.empty)((dl,a) => a +: dl)
    dl.headOption == Some(1)
  }
}
