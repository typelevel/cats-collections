/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.collections

import cats.syntax.all._
import cats.collections.arbitrary.ArbitraryChunkedSeq._
import cats.laws.discipline._
import cats.{Eq, Monoid, Order, PartialOrder, Traverse}
import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen, Gen, Test}

class ChunkedSeqSuite extends DisciplineSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default

  implicit def arbPartialFn[A: Cogen, B: Arbitrary]: Arbitrary[PartialFunction[A, B]] =
    Arbitrary(Gen.zip(Gen.choose(0, 32), Gen.choose(Int.MinValue, Int.MaxValue), Arbitrary.arbitrary[A => B]).map {
      case (shift, xor, fn) => { case a if (a.hashCode ^ xor) >>> shift == 0 => fn(a) }
    })

  checkAll("Traverse[ChunkedSeq]", TraverseTests[ChunkedSeq].traverse[Long, Int, String, Int, Option, Option])

  checkAll("Alternative[ChunkedSeq]", AlternativeTests[ChunkedSeq].alternative[Long, Int, String])

  checkAll("FunctorFilter[ChunkedSeq]", FunctorFilterTests[ChunkedSeq].functorFilter[Long, Int, String])

  checkAll("Monad[ChunkedSeq]", MonadTests[ChunkedSeq].monad[Long, Int, String])

  checkAll("CoflatMap[ChunkedSeq]", CoflatMapTests[ChunkedSeq].coflatMap[Long, Int, String])

  checkAll("Traverse[ChunkedSeq]", SerializableTests.serializable(Traverse[ChunkedSeq]))

  private def testHomomorphism[A, B: Eq](as: ChunkedSeq[A])(fn: ChunkedSeq[A] => B, gn: List[A] => B): Unit = {
    val la = as.toList
    assert(Eq[B].eqv(fn(as), gn(la)))
  }

  property("iterator works")(forAll { (xs: ChunkedSeq[Int]) =>
    assertEquals(ChunkedSeq.fromList(xs.toIterator.toList), xs)
  })

  property("++ works")(forAll { (xs: ChunkedSeq[Int], ys: ChunkedSeq[Int]) =>
    testHomomorphism(xs)({ l => (l ++ ys).toList }, { _ ++ ys.toList })
  })

  property("drop/take work")(forAll { (xs: ChunkedSeq[Int], n: Int) =>
    testHomomorphism(xs)({ _.drop(n.toLong).toList }, { _.drop(n) })
    testHomomorphism(xs)({ _.take(n.toLong).toList }, { _.take(n) })
    // we should be able to drop for all sizes:
    (-1L to xs.size).foreach { cnt =>
      assertEquals(xs.drop(cnt).toList, xs.toList.drop(cnt.toInt))
      assertEquals(xs.take(cnt) ++ xs.drop(cnt), xs)
    }
  })

  property("lastOption works")(forAll { (xs: ChunkedSeq[Int]) =>
    testHomomorphism(xs)({ _.lastOption }, { _.lastOption })
  })

  property("toReverseIterator works")(forAll { (xs: ChunkedSeq[Int]) =>
    testHomomorphism(xs)({ _.toReverseIterator.toList }, { _.reverse })
  })

  test("reverse works")(forAll { (xs: ChunkedSeq[Int]) =>
    assertEquals(xs.reverse.toList, xs.toReverseIterator.toList)
  })

  property("strictFoldRight works")(forAll { (xs: ChunkedSeq[Int], init: String, fn: (Int, String) => String) =>
    testHomomorphism(xs)({ _.strictFoldRight(init)(fn) }, { _.foldRight(init)(fn) })
  })

  property("fromList/toList works")(forAll { (xs: List[Int]) =>
    assertEquals(ChunkedSeq.fromList(xs).toIterator.toList, xs)
    assertEquals(ChunkedSeq.fromList(xs).toList, xs)
  })

  property("size works")(forAll { (xs: ChunkedSeq[Int]) =>
    testHomomorphism(xs)({ _.size }, { _.size.toLong })
  })

  property("pattern matching works")(forAll { (xs: ChunkedSeq[Int]) =>
    xs match {
      case ChunkedSeq.NonEmpty(head, tail) =>
        assert(xs.nonEmpty)
        assertEquals(Option((head, tail)), xs.uncons)
        assertEquals(Option(head), xs.headOption)
        assertEquals(Option(tail), xs.tailOption)
      case _ =>
        assert(xs.isEmpty)
        assertEquals(xs, ChunkedSeq.empty[Int])
        assertEquals(xs.uncons, None)
        assertEquals(xs.headOption, None)
        assertEquals(xs.tailOption, None)
    }
  })

  sealed private trait Opaque1
  private object Opaque1 {
    private case class OI(toInt: Int) extends Opaque1
    implicit val eqOpaque: Eq[Opaque1] =
      Eq[Int].contramap[Opaque1] { case OI(i) => i }

    implicit val arbO1: Arbitrary[Opaque1] =
      Arbitrary(Arbitrary.arbitrary[Int].map(OI(_)))
  }

  property("Eq[ChunkedSeq[A]] works")(forAll { (xs: ChunkedSeq[Opaque1], ys: ChunkedSeq[Opaque1]) =>
    assertEquals(Eq[ChunkedSeq[Opaque1]].eqv(xs, ys), Eq[List[Opaque1]].eqv(xs.toList, ys.toList))
    assert(Eq[ChunkedSeq[Opaque1]].eqv(xs, xs))
  })

  property("Order[ChunkedSeq[A]] works")(forAll { (xs: ChunkedSeq[Int], ys: ChunkedSeq[Int]) =>
    assertEquals(Order[ChunkedSeq[Int]].compare(xs, ys), Order[List[Int]].compare(xs.toList, ys.toList))
    assertEquals(Order[ChunkedSeq[Int]].compare(xs, xs), 0)
  })

  sealed private trait Opaque2
  private object Opaque2 {
    private case class OI(toInt: Int) extends Opaque2
    implicit val partialOrd: PartialOrder[Opaque2] =
      PartialOrder[Int].contramap[Opaque2] { case OI(i) => i }

    implicit val arbO1: Arbitrary[Opaque2] =
      Arbitrary(Arbitrary.arbitrary[Int].map(OI(_)))
  }

  property("PartialOrder[ChunkedSeq[A]] works")(forAll { (xs: ChunkedSeq[Opaque2], ys: ChunkedSeq[Opaque2]) =>
    assertEquals(
      PartialOrder[ChunkedSeq[Opaque2]].partialCompare(xs, ys),
      PartialOrder[List[Opaque2]].partialCompare(xs.toList, ys.toList)
    )
    assertEquals(PartialOrder[ChunkedSeq[Opaque2]].partialCompare(xs, xs), 0.0)
  })

  property("Monoid[ChunkedSeq[A]].combine works")(forAll { (xs: ChunkedSeq[Int], ys: ChunkedSeq[Int]) =>
    assertEquals(Monoid[ChunkedSeq[Int]].combine(xs, ys), xs ++ ys)
  })

  test("Monoid[ChunkedSeq[A]].empty works") {
    assertEquals(Monoid[ChunkedSeq[Int]].empty, ChunkedSeq.empty[Int])
  }

  property("toString is as expected")(forAll { (xs: ChunkedSeq[Int]) =>
    assertEquals(xs.toString, xs.toIterator.mkString("ChunkedSeq(", ", ", ")"))
  })

  property("ChunkedSeq.get works")(forAll { (xs: ChunkedSeq[Int]) =>
    assertEquals(xs.get(-1L), None)
    assertEquals(xs.get(xs.size), None)
    intercept[NoSuchElementException](xs.getUnsafe(-1L))
    intercept[NoSuchElementException](xs.getUnsafe(xs.size))

    val list = xs.toList
    (0L until xs.size).foreach { idx =>
      assertEquals(xs.get(idx), Some(list(idx.toInt)))
      assertEquals(xs.getUnsafe(idx), list(idx.toInt))
    }
  })

  property("toIterator throws the same type of exception as List on empty")(forAll { (xs: ChunkedSeq[Int]) =>
    val it = xs.toIterator
    // exhaust the iterator
    it.size
    intercept[NoSuchElementException](Nil.iterator.next())
    intercept[NoSuchElementException](it.next())
    ()
  })

  property("toReverseIterator throws the same type of exception as List on empty")(forAll { (xs: ChunkedSeq[Int]) =>
    val it = xs.toReverseIterator
    // exhaust the iterator
    it.size
    intercept[NoSuchElementException](Nil.iterator.next())
    intercept[NoSuchElementException](it.next())
    ()
  })

  property("ChunkedSeq.NonEmpty.apply/unapply are inverses")(forAll { (head: Int, tail: ChunkedSeq[Int]) =>
    ChunkedSeq.NonEmpty(head, tail) match {
      case ChunkedSeq.NonEmpty(h, t) =>
        assertEquals(h, head)
        assertEquals(t, tail)
      case other =>
        fail(s"unreachable: $other")
    }
  })

  // looks like cats is not testing this
  property("ChunkedSeq.traverse_/traverse consistency")(forAll { (xs: ChunkedSeq[Int], fn: Int => Option[String]) =>
    assertEquals(xs.traverse(fn).void, xs.traverse_(fn))
  })

  // looks like cats is not testing this
  property("ChunkedSeq.sequence_/sequence consistency")(forAll { (xs: ChunkedSeq[Option[Int]]) =>
    assertEquals(xs.sequence.void, xs.sequence_)
  })

  test("Show matches toString")(forAll { (xs: ChunkedSeq[Int]) =>
    assertEquals(xs.show, xs.toString)
  })

  property("lastOption matches get(size - 1L)")(forAll { (xs: ChunkedSeq[Int]) =>
    assertEquals(xs.get(xs.size - 1L), xs.lastOption)
  })

  property("toListReverse == toList.reverse")(forAll { (xs: ChunkedSeq[Int]) =>
    assertEquals(xs.toListReverse, xs.toList.reverse)
  })

  property("updated works")(forAll { (xs: ChunkedSeq[Int], v: Int, idx0: Long) =>
    def test(idx: Long): Unit = {
      val xs1 = xs.updatedOrThis(idx, v)
      if (0 <= idx && idx < xs.size && xs.size > 0) {
        val ls = xs.toIterator.toVector
        val ls1 = ls.updated(idx.toInt, v)
        assertEquals(xs1.toIterator.toVector, ls1)
        assertEquals(xs.updated(idx, v), Some(xs1))
      } else {
        assertEquals(xs, xs1)
        assertEquals(xs.updated(idx, v), None)
      }
    }

    val idx = if (xs.size > 0) idx0 % xs.size else idx0
    test(idx)
    // also test all valid lengths:
    (-1L to xs.size).foreach(test)
  })

  test("we don't stack overflow on large sequences") {
    val size = 100000
    def buildFn(i: Int): Int => Int = { (j: Int) => i + j }
    val bigList = (0 until size).iterator.map(buildFn).toList
    // Now this should not throw
    val bigChunkedSeq = ChunkedSeq.fromList(bigList)
    assertEquals(bigChunkedSeq.sequence.apply(0), ChunkedSeq.fromList((0 until size).toList))
  }

  property("filter/filterNot consistency")(forAll { (xs: ChunkedSeq[Int], fn: Int => Boolean) =>
    testHomomorphism(xs)({ l => l.filter(fn).toList }, { _.filter(fn) })
    assertEquals(xs.filterNot(fn), xs.filter { a => !fn(a) })
  })

  property("prepend works")(forAll { (xs: List[Int]) =>
    val cs = xs.foldRight(ChunkedSeq.empty[Int])((a, acc) => a :: acc)
    assertEquals(cs.toList, xs)
  })

  property("append works")(forAll { (xs: List[Int]) =>
    val cs = xs.foldLeft(ChunkedSeq.empty[Int])((acc, a) => acc :+ a)
    assertEquals(cs.toList, xs)
  })

  property("uncons/unsnoc consistency")(forAll { (xs: ChunkedSeq[Int]) =>
    xs.uncons match {
      case Some((head, tail)) =>
        assertEquals(head, xs.headOption.get)
        assertEquals(tail.size, xs.size - 1L)
      case None =>
        assert(xs.isEmpty)
    }
    xs.unsnoc match {
      case Some((init, last)) =>
        assertEquals(last, xs.lastOption.get)
        assertEquals(init.size, xs.size - 1L)
      case None =>
        assert(xs.isEmpty)
    }
  })

  property("foldLeft via uncons is consistent")(forAll {
    (xs: ChunkedSeq[Int], init: String, fn: (String, Int) => String) =>
      val expected = xs.toList.foldLeft(init)(fn)
      assertEquals(xs.foldLeft(init)(fn), expected)
  })

  test("we don't stack overflow on foldLeft with deep tree") {
    val size = 100000
    // Build a deep left-leaning tree via prepend
    var cs: ChunkedSeq[Int] = ChunkedSeq.empty
    (0 until size).foreach { i => cs = i :: cs }
    assertEquals(cs.foldLeft(0)(_ + _), (0 until size).sum)
  }

  test("we don't stack overflow on iteration with deep tree") {
    val size = 100000
    var cs: ChunkedSeq[Int] = ChunkedSeq.empty
    (0 until size).foreach { i => cs = cs :+ i }
    assertEquals(cs.toIterator.sum, (0 until size).sum)
  }
}
