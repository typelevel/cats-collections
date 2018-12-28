package cats.collections
package tests

import cats.{Eq, Order, PartialOrder, Monoid, Traverse}
import cats.laws.discipline._
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Cogen}

import cats.implicits._

class TreeListSpec extends CatsSuite {
  implicit def arbitraryTreeList[A: Arbitrary]: Arbitrary[TreeList[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(TreeList.fromListReverse(_)))

  implicit def cogenTreeList[A: Cogen]: Cogen[TreeList[A]] =
    Cogen[List[A]].contramap(_.toList)

  checkAll("Traverse[TreeList]",
    TraverseTests[TreeList].traverse[Long, Int, String, Int, Option, Option])

  checkAll("Alternative[TreeList]",
    AlternativeTests[TreeList].alternative[Long, Int, String])

  checkAll("Monad[TreeList]",
    MonadTests[TreeList].monad[Long, Int, String])

  checkAll("CoflatMap[TreeList]",
    CoflatMapTests[TreeList].coflatMap[Long, Int, String])

  checkAll("Traverse[TreeList]", SerializableTests.serializable(Traverse[TreeList]))

  test("iterator works")(forAll { xs: TreeList[Int] =>
    assert(TreeList.fromList(xs.toIterator.toList) == xs)
  })

  private def testHomomorphism[A, B: Eq](as: TreeList[A])(fn: TreeList[A] => B, gn: List[A] => B) = {
    val la = as.toList
    assert(Eq[B].eqv(fn(as), gn(la)))
  }

  test("++ works")(forAll { (xs: TreeList[Int], ys: TreeList[Int]) =>
    testHomomorphism(xs)({ l => (l ++ ys).toList }, { _ ++ (ys.toList) })
  })

  test("drop works")(forAll { (xs: TreeList[Int], n: Int) =>
    testHomomorphism(xs)({ _.drop(n.toLong).toList }, { _.drop(n) })
  })

  test("lastOption works")(forAll { (xs: TreeList[Int]) =>
    testHomomorphism(xs)({ _.lastOption }, { _.lastOption })
  })

  test("toReverseIterator works")(forAll { (xs: TreeList[Int]) =>
    testHomomorphism(xs)({ _.toReverseIterator.toList }, { _.reverse })
  })

  test("reverse works")(forAll { (xs: TreeList[Int]) =>
    assert(xs.reverse.toList == xs.toReverseIterator.toList)
  })

  test("strictFoldRight works")(forAll { (xs: TreeList[Int], init: String, fn: (Int, String) => String) =>
    testHomomorphism(xs)({ _.strictFoldRight(init)(fn) }, { _.foldRight(init)(fn) })
  })

  test("fromList/toList works")(forAll { xs: List[Int] =>
    assert(TreeList.fromList(xs).toIterator.toList == xs)
    assert(TreeList.fromList(xs).toList == xs)
  })

  test("size works")(forAll { xs: TreeList[Int] =>
    testHomomorphism(xs)({ _.size }, { _.size.toLong })
  })

  test("split combined is identity")(forAll { (xs: TreeList[Int]) =>
    val (left, right) = xs.split
    assert((left.toList ::: right.toList) == xs.toList)
  })

  test("split is roughly in half")(forAll { (xs: TreeList[Int]) =>
    val (left, right) = xs.split
    val leftSize = left.size
    val rightSize = right.size
    // right size is 2^n - 1
    val shifts = java.lang.Long.bitCount(rightSize)
    assert(rightSize >> shifts == 0L)
  })

  test("pattern matching works")(forAll { (xs: TreeList[Int]) =>
    xs match {
      case TreeList.Empty => assert(xs.isEmpty)
      case TreeList.NonEmpty(head, tail) =>
        assert(xs.nonEmpty)
        assert(Some((head, tail)) == xs.uncons)
    }
  })

  test("maxDepth <= 2 log_2 N + 1")(forAll { (xs: TreeList[Int]) =>
    val maxD = xs.maxDepth
    if (xs.isEmpty) assert(maxD == 0)
    else {
      val upper = 2.0 * math.log(xs.size.toDouble)/math.log(2.0) + 1.0
      assert(maxD.toDouble <= upper)
    }
  })

  test("Eq[TreeList[A]] works")(forAll { (xs: TreeList[Int], ys: TreeList[Int]) =>
    assert(Eq[TreeList[Int]].eqv(xs, ys) == Eq[List[Int]].eqv(xs.toList, ys.toList))
    assert(Eq[TreeList[Int]].eqv(xs, xs) == true)
  })

  test("Order[TreeList[A]] works")(forAll { (xs: TreeList[Int], ys: TreeList[Int]) =>
    assert(Order[TreeList[Int]].compare(xs, ys) == Order[List[Int]].compare(xs.toList, ys.toList))
    assert(Order[TreeList[Int]].compare(xs, xs) == 0)
  })

  test("PartialOrder[TreeList[A]] works")(forAll { (xs: TreeList[Int], ys: TreeList[Int]) =>
    assert(PartialOrder[TreeList[Int]].partialCompare(xs, ys) == PartialOrder[List[Int]].partialCompare(xs.toList, ys.toList))
    assert(PartialOrder[TreeList[Int]].partialCompare(xs, xs) == 0.0)
  })

  test("Monoid[TreeList[A]].combine works")(forAll { (xs: TreeList[Int], ys: TreeList[Int]) =>
    assert(Monoid[TreeList[Int]].combine(xs, ys) == xs ++ ys)
  })

  test("Monoid[TreeList[A]].empty works") {
    assert(Monoid[TreeList[Int]].empty eq TreeList.empty)
  }
}
