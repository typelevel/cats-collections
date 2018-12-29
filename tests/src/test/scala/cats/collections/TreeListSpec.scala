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

  test("drop/take work")(forAll { (xs: TreeList[Int], n: Int) =>
    testHomomorphism(xs)({ _.drop(n.toLong).toList }, { _.drop(n) })
    testHomomorphism(xs)({ _.take(n.toLong).toList }, { _.take(n) })
    // we should be able to drop for all sizes:
    (-1L to xs.size).foreach { cnt =>
      assert(xs.drop(cnt).toList == xs.toList.drop(cnt.toInt))
      assert((xs.take(cnt) ++ xs.drop(cnt)) == xs)
    }
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

  test("split produces a full left tree")(forAll { (xs: TreeList[Int]) =>
    val (_, right) = xs.split
    val rightSize = right.size
    // right size is 2^n - 1
    val shifts = java.lang.Long.bitCount(rightSize)
    // since we have shifted all the bits, all of them must have been in the right most
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

  trait Opaque1
  object Opaque1 {
    private case class OI(toInt: Int) extends Opaque1
    implicit val eqOpaque: Eq[Opaque1] =
      Eq[Int].contramap[Opaque1] { case OI(i) => i }

    implicit val arbO1: Arbitrary[Opaque1] =
      Arbitrary(Arbitrary.arbitrary[Int].map(OI(_)))
  }

  test("Eq[TreeList[A]] works")(forAll { (xs: TreeList[Opaque1], ys: TreeList[Opaque1]) =>
    assert(Eq[TreeList[Opaque1]].eqv(xs, ys) == Eq[List[Opaque1]].eqv(xs.toList, ys.toList))
    assert(Eq[TreeList[Opaque1]].eqv(xs, xs) == true)
  })

  test("Order[TreeList[A]] works")(forAll { (xs: TreeList[Int], ys: TreeList[Int]) =>
    assert(Order[TreeList[Int]].compare(xs, ys) == Order[List[Int]].compare(xs.toList, ys.toList))
    assert(Order[TreeList[Int]].compare(xs, xs) == 0)
  })

  trait Opaque2
  object Opaque2 {
    private case class OI(toInt: Int) extends Opaque2
    implicit val partialOrd: PartialOrder[Opaque2] =
      PartialOrder[Int].contramap[Opaque2] { case OI(i) => i }

    implicit val arbO1: Arbitrary[Opaque2] =
      Arbitrary(Arbitrary.arbitrary[Int].map(OI(_)))
  }

  test("PartialOrder[TreeList[A]] works")(forAll { (xs: TreeList[Opaque2], ys: TreeList[Opaque2]) =>
    assert(PartialOrder[TreeList[Opaque2]].partialCompare(xs, ys) == PartialOrder[List[Opaque2]].partialCompare(xs.toList, ys.toList))
    assert(PartialOrder[TreeList[Opaque2]].partialCompare(xs, xs) == 0.0)
  })

  test("Monoid[TreeList[A]].combine works")(forAll { (xs: TreeList[Int], ys: TreeList[Int]) =>
    assert(Monoid[TreeList[Int]].combine(xs, ys) == xs ++ ys)
  })

  test("Monoid[TreeList[A]].empty works") {
    assert(Monoid[TreeList[Int]].empty eq TreeList.empty)
  }

  test("toString is as expected")(forAll { (xs: TreeList[Int]) =>
    assert(xs.toString == xs.toIterator.mkString("TreeList(", ", ", ")"))
  })

  test("TreeList.get works")(forAll { (xs: TreeList[Int]) =>
    assert(xs.get(-1L) == None)
    assert(xs.get(xs.size) == None)

    val list = xs.toList
    (0L until xs.size).foreach { idx =>
      assert(xs.get(idx) == Some(list(idx.toInt)))
    }
  })

  test("toIterator throws the same type of exception as List on empty")(forAll { (xs: TreeList[Int]) =>
    val it = xs.toIterator
    // exhaust the iterator
    it.size
    assertThrows[NoSuchElementException] { Nil.iterator.next }
    assertThrows[NoSuchElementException] {
      it.next
    }
  })

  test("toReverseIterator throws the same type of exception as List on empty")(forAll { (xs: TreeList[Int]) =>
    val it = xs.toReverseIterator
    // exhaust the iterator
    it.size
    assertThrows[NoSuchElementException] { Nil.iterator.next }
    assertThrows[NoSuchElementException] {
      it.next
    }
  })

  test("TreeList.NonEmpty.apply/unapply are inverses")(forAll { (head: Int, tail: TreeList[Int]) =>
    TreeList.NonEmpty(head, tail) match {
      case TreeList.Empty => fail("should not be empty")
      case TreeList.NonEmpty(h, t) =>
        assert(h == head)
        assert(t == tail)
      case other =>
        fail(s"unreachable: $other")
    }
  })

  // looks like cats is not testing this
  test("TreeList.traverse_/traverse consistency")(forAll { (xs: TreeList[Int], fn: Int => Option[String]) =>
    assert(xs.traverse(fn).void == xs.traverse_(fn))
  })

  // looks like cats is not testing this
  test("TreeList.sequence_/sequence consistency")(forAll { (xs: TreeList[Option[Int]]) =>
    assert(xs.sequence.void == xs.sequence_)
  })

  test("Show matches toString")(forAll{ (xs: TreeList[Int]) =>
    assert(xs.show == xs.toString)
  })

  test("lastOption matches get(size - 1L)")(forAll { (xs: TreeList[Int]) =>
    assert(xs.get(xs.size - 1L) == xs.lastOption)
  })

  test("toListReverse == toList.reverse")(forAll { (xs: TreeList[Int]) =>
    assert(xs.toListReverse == xs.toList.reverse)
  })
}
