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
import cats.collections.arbitrary.blist._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.{Eq, Eval, Traverse}
import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.scalacheck.Test
import scala.math.pow
import scala.util.Random
import cats.data.State

class BListSuite extends DisciplineSuite {
  override def scalaCheckInitialSeed = "7bTNcKSrPdvUlNm_fJry4aTO89pk7TzTVJOnhKCdlWL="
  override def scalaCheckTestParameters: Test.Parameters =
    // DefaultScalaCheckPropertyCheckConfig.default
    super.scalaCheckTestParameters.withMaxSize(BList.BlockSize * 5)

  checkAll("BList.FunctorLaws", FunctorTests[BList].functor[Int, Int, String])
  checkAll("BList.SemigroupKLaws", SemigroupKTests[BList].semigroupK[Int])
  checkAll("BList.FoldableLaws", FoldableTests[BList].foldable[Int, Long])
  checkAll("BList.ApplicativeLaws", ApplicativeTests[BList].applicative[Int, Int, Int])
  checkAll("BList.MonoidKLaws", MonoidKTests[BList].monoidK[Int])
  checkAll("BList.AlternativeLaws", AlternativeTests[BList].alternative[Int, Int, Int])
  checkAll("BList.TraverseLaws", TraverseTests[BList].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("BList.TraverseLaws with Stacksafe Monad",
           TraverseTests[BList].traverse[Int, Int, Int, Set[Int], Eval, Eval]
  )
  checkAll("BList.MonadLaws", MonadTests[BList].monad[Int, Int, Int])
  checkAll("BList.NonEmpty.NonEmptyAlternativeLaws",
           NonEmptyAlternativeTests[BList.NonEmpty].nonEmptyAlternative[Int, Int, Int]
  )
  checkAll("BList.NonEmpty.NonEmptyTraverseLaws",
           NonEmptyTraverseTests[BList.NonEmpty].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll("Traverse[TreeList]", SerializableTests.serializable(Traverse[BList]))


  test("concatenating to form same list") {
    val l1 = BList.empty[Int].prepend(5).prepend(4).prepend(3).prepend(2).prepend(1)
    val m1 = BList.empty[Int].prepend(0).prepend(-1).prepend(-2)

    val l2 = BList.empty[Int].prepend(5).prepend(4).prepend(3).prepend(2)
    val m2 = BList.empty[Int].prepend(1).prepend(0).prepend(-1).prepend(-2)

    val n1 = m1.concat(l1)
    val n2 = m2.concat(l2)
    assertEquals(n1, n2)
  }

  test("uncons when tail is a new node") {
    val l = BList.empty[Int].prepend(4).prepend(3).prepend(2).prepend(1)
    val m = BList.empty[Int].prepend(0)

    val n = m.concat(l)

    n.uncons match {
      // case None         => fail("should not be empty")
      case Some((h, t)) =>
        assertEquals(h, m.head)
        assertEquals(t, l)
    }
  }

  test("prepend when block is full") {
    var l = BList.empty[Int]
    for (i <- (0 until BList.BlockSize).reverse) {
      l = l.prepend(i)
    }
    val m = l.prepend(-1)
    assertEquals(m.toList, (-1 until BList.BlockSize).toList)
  }

  test("headOption on empty list") {
    val l = BList.empty[Int]
    assertEquals(l.headOption, None)
  }

  test("headOption on single element") {
    val l = BList.empty[Int].prepend(1)
    assertEquals(l.headOption, Some(1))
  }

  test("tailOption on single element") {
    val l = BList.empty[Int].prepend(1)
    assertEquals(l.tailOption, Some(BList.empty[Int]))
  }

  test("tail on single element") {
    val l = BList.NonEmpty[Int](1, BList.empty[Int])
    assertEquals(l.tail, BList.empty[Int])
  }

  test("get/getUnsafe on a Blist with incomplete blocks") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until BList.BlockSize).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    assertEquals(m.getUnsafe((BList.BlockSize + (BList.BlockSize - 1)).toLong), 2)
    assertEquals(m.get((BList.BlockSize + (BList.BlockSize - 1)).toLong), Some(2))
  }
  test("get/getUnsafe index too small") {
    val l = BList.empty[Int].prepend(5).prepend(4).prepend(3).prepend(2).prepend(1)
    assertEquals(l.get(-1), None)
    intercept[IndexOutOfBoundsException](l.getUnsafe(-1))
  }
  test("get/getUnsafe index 0 on empty list") {
    val l = BList.empty[Int]
    assertEquals(l.get(0), None)
    intercept[IndexOutOfBoundsException](l.getUnsafe(0))
  }
  test("get/getUnsafe index too big") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until BList.BlockSize).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    // size is triangle numbers in BList.BlockSize. size should be  n(n+1)/2 where n is BList.BlockSize
    // so BList.BlockSize ** 2 + 1 will always be out of bounds
    val idx: Long = pow(BList.BlockSize.toDouble, 2.0).toLong + 1L
    assertEquals(m.get(idx), None)
    intercept[IndexOutOfBoundsException](m.getUnsafe(idx))
  }
  test("lastOption with incomplete blocks") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until BList.BlockSize).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }

    // last should be blocksize-1
    assertEquals(m.lastOption, Some(BList.BlockSize - 1))
  }
  test("lastOption on empty list") {
    val l = BList.empty[Int]
    assertEquals(l.lastOption, None)
  }
  test("size on a triangle number construction") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until BList.BlockSize).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    // size is triangle numbers in BList.BlockSize. formula: n(n+1)/2
    val expected: Long = (BList.BlockSize * (BList.BlockSize + 1)).toLong / 2L
    assertEquals(m.size, expected)
  }
  test("size on empty") {
    val l = BList.empty[Int]
    assertEquals(l.size, 0L)
  }

  test("prepend and uncons are inverses") {
    val l = BList.empty[Int].prepend(4).prepend(3).prepend(2).prepend(1)
    val m = l.prepend(0)

    m.uncons match {
      // case None         => fail("should not be empty")
      case Some((h, t)) =>
        assertEquals(h, 0)
        assertEquals(t, l)
    }
  }

  test("simple map") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until 10).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    val mapped = m.map(_ + 10)
    assertEquals(mapped.getUnsafe(20L), m.getUnsafe(20L) + 10)
    assertEquals(mapped.getUnsafe(25L), m.getUnsafe(25L) + 10)
  }
  test("map to a different type") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until 10).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }

    val mapped = m.map(_ >= 3)
    assertEquals(mapped.getUnsafe(20L), m.getUnsafe(20L) >= 3)
    assertEquals(mapped.getUnsafe(25L), m.getUnsafe(25L) >= 3)
  }

  test("map on empty") {
    val l = BList.empty[Int]
    assertEquals(l.map(_ + 100), l)
  }

  test("foldleft with subtraction") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until 10).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    assertEquals(m.foldLeft(m.foldLeft(0)((acc: Int, x: Int) => acc + x))((acc: Int, x: Int) => acc - x), 0)
  }

  test("drop on fragmented list") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until 10).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    assertEquals(m.drop(10).getUnsafe(15), m.getUnsafe(25))
    assertEquals(m.drop(10).getUnsafe(20), m.getUnsafe(30))
    assertEquals(m.drop(20).getUnsafe(5), m.getUnsafe(25))
  }

  test("drop everything") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until 4).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    assertEquals(m.drop(10), BList.empty[Int])
    assertEquals(m.drop(20), BList.empty[Int])
    assertEquals(m.drop(1000), BList.empty[Int])
  }
  test("drop nothing") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until 4).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }

    assertEquals(m.drop(0), m)
    assertEquals(m.drop(-10), m)
    assertEquals(m.drop(-200), m)
  }

  test("blocks don't matter for equality") {
    // todo
    val l1 = BList.empty[Int].prepend(3).prepend(2).prepend(1)
    val m1 = BList.empty[Int].prepend(0).prepend(-1).prepend(-2)

    val l2 = BList.empty[Int].prepend(3).prepend(2)
    val m2 = BList.empty[Int].prepend(1).prepend(0).prepend(-1).prepend(-2)

    val n1 = m1.concat(l1)
    val n2 = m2.concat(l2)
    assertEquals(n1, n2)

  }
  test("BList constructor") {
    val l1 = BList.empty[Int].prepend(7).prepend(6).prepend(5).prepend(4).prepend(3).prepend(2).prepend(1)
    val l2 = BList(1, 2, 3, 4, 5, 6, 7)
    assertEquals(l1, l2)
  }

  test("stack safety") { // this test might be pretty slow so maybe we remove it
    var l = BList.empty[Int]
    var m = BList.empty[Int]

    for (i <- (0 until 3000)) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    m.filter(x=> x%2==0)
    m.filterNot(x=> x%2==0)
    m.splitAt(m.size.toInt - 2)
    m.take(m.size.toInt - 2)
    m.takeEval(m.size.toInt - 2) //remove later
    m.takeWhile(_<1001)
    m.collect{case x if x < 1001 => x*2}
    m.concat(BList(1,2,3))
    m.get(m.size - 2L)
    m.map(x=>x.toString)
    m.foldLeft(0)((x,y)=> x+y)
    m.drop(1000)
    m.dropWhile(_<999)
    m.toList
    m.toListReverse
    m.asSeq
    m.flatMap(x=>BList(1,x,3))
    m.iterator
    m.toString
    m.foldRight(Eval.now(0)) { (item, evalAcc) => evalAcc.map(_ + item)}
    m.traverseVoid(x=> if (x<1001) Some(x) else None)
    m.traverse(x=> if (x<1001) Some(x) else None)
  }

  private def testHomomorphism[A, B: Eq](as: BList[A])(fn: BList[A] => B, gn: List[A] => B): Unit = {
    val la = as.toList
    assert(Eq[B].eqv(fn(as), gn(la)))
  }

  property("size works")(forAll { (xs: BList[Int]) =>
    testHomomorphism(xs)({ _.size }, { _.size.toLong })
  })
  property("last is the same as get(size-1)")(forAll { (xs: BList[Int]) =>
    assertEquals(xs.lastOption, xs.get(xs.size - 1L))
  })

  property("concat works")(forAll { (xs: BList[Int], ys: BList[Int]) =>
    testHomomorphism(xs)({ l => l.concat(ys).toList }, { _ ++ ys.toList })
  })

  property("get and getUnsafe are consistent")(forAll { (xs: BList[Int]) =>
    assertEquals(xs.get(-1L), None)
    assertEquals(xs.get(xs.size), None)
    intercept[IndexOutOfBoundsException](xs.getUnsafe(-1L))
    intercept[IndexOutOfBoundsException](xs.getUnsafe(xs.size))

    val list = xs.toList
    (0L until xs.size).foreach { idx =>
      assertEquals(xs.get(idx), Some(list(idx.toInt)))
      assertEquals(xs.getUnsafe(idx), list(idx.toInt))
    }
  })

  property("prepending head on to tail makes the same list")(forAll { (xs: BList[Int]) =>
    xs.headOption match {
      case Some(h) =>
        xs.tailOption match {
          case Some(t) => assertEquals(t.prepend(h), xs)
          case None    => assertEquals(BList.empty.prepend(h), xs)
        }
      case None => assertEquals(xs, BList.empty)
    }
  })

  property("headoption, head consistent and tailoption, tail consistent")(forAll { (xs: BList[Int]) =>
    xs.headOption match {
      case Some(h) => assertEquals(h, xs.asInstanceOf[BList.NonEmpty[Int]].head)
      case None    => // head wont work because xs is the empty list
    }
    xs.tailOption match {
      case Some(t) => assertEquals(t, xs.asInstanceOf[BList.NonEmpty[Int]].tail)
      case None    => // tail wont work because xs is the empty list
    }
  })

  property("headOption works")(forAll { (xs: BList[Int]) =>
    testHomomorphism(xs)({ _.headOption }, { _.headOption })
  })

  property("lastOption works")(forAll { (xs: BList[Int]) =>
    testHomomorphism(xs)({ _.lastOption }, { _.lastOption })
  })

  property("toList inverse of fromList")(forAll { (xs: BList[Int]) =>
    assertEquals(xs, BList.fromList(xs.toList))
  })

  property("pattern matching works")(forAll { (xs: BList[Int]) =>
    xs match {
      case BList.NonEmpty(h, t) =>
        assertEquals(xs.headOption, Option(h))
        assertEquals(xs.asInstanceOf[BList.NonEmpty[Int]].tail, t)
      case BList.Empty =>
        assertEquals(xs, BList.Empty)
        assertEquals(xs.uncons, None)
        assertEquals(xs.headOption, None)
        assertEquals(xs.tailOption, None)
        assertEquals(xs.lastOption, None)
    }
  })

  property("apply/unapply are inverses for NonEmpty")(forAll { (head: Int, tail: BList[Int]) =>
    BList.NonEmpty(head, tail) match {
      case BList.NonEmpty(h, t) =>
        assertEquals(h, head)
        assertEquals(t, tail)
    }
  })

  property("fromListReverse == .reverse fromList")(forAll { (xs: List[Int]) =>
    assertEquals(BList.fromListReverse(xs), BList.fromList(xs.reverse))
  })

  property("concat is associative")(forAll { (xs: BList[Int], ys: BList[Int], zs: BList[Int]) =>
    val l1 = xs.concat(ys).concat(zs)
    val l2 = xs.concat(ys.concat(zs))
    assertEquals(l1, l2)
  })
  property("== implies same toString for int lists")(forAll { (xs: BList[Int], ys: BList[Int]) =>
    if (xs.toString == ys.toString) {
      assertEquals(xs, ys)
    }
  })

  property("toListReverse same as toList.reverese")(forAll { (xs: BList[Int]) =>
    assertEquals(xs.toList.reverse, xs.toListReverse)
  })

  property("flatmap works")(forAll { (xs: BList[Int], f: Int => BList[Int]) =>
    assertEquals(xs.flatMap(f).toList, xs.toList.flatMap((x: Int) => f(x).toList))
  })

  property("filter and filterIter do the same thing")(forAll { (xs: BList[Int], p: Int => Boolean) =>
    assertEquals(xs.filter(p), xs.filterIter(p))
  })

  property("BList filter consistent with List filter")(forAll { (xs: BList[Int], p: Int => Boolean) =>
    assertEquals(xs.filter(p).toList, xs.toList.filter(p))
  })

  property("BList splitAt consistent with List splitAt")(forAll { (xs: BList.NonEmpty[Int]) =>
    val index = Random.nextInt(xs.size.toInt)
    val (l1, l2) = xs.toList.splitAt(index)
    val (b1, b2) = xs.splitAt(index)
    assertEquals(b1.toList, l1)
    assertEquals(b2.toList, l2)
  })

  property("BList takeWhile consistent with List")(forAll { (xs: BList[Int], p: Int => Boolean) =>
    assertEquals(xs.takeWhile(p).toList, xs.toList.takeWhile(p))
  })
  property("BList take consistent with List")(forAll { (xs: BList[Int]) =>
    val n = Random.nextInt(xs.size.toInt + 10) - 5
    assertEquals(xs.take(n).toList, xs.toList.take(n))
    assertEquals(xs.take(n), xs.takeEval(n))
  })
  property("take consistent with takeEval")(forAll { (xs: BList[Int]) =>
    val n = Random.nextInt(xs.size.toInt + 10) - 5
    assertEquals(xs.take(n), xs.takeEval(n))
  })

  property("BList dropWhile consistent with List")(forAll { (xs: BList[Int], p: Int => Boolean) =>
    assertEquals(xs.dropWhile(p).toList, xs.toList.dropWhile(p))
  })

  property("BList filter consistent with List")(forAll { (xs: BList[Int], p: Int => Boolean) =>
    assertEquals(xs.filter(p).toList, xs.toList.filter(p))
  })
  property("BList filterNot consistent with List")(forAll { (xs: BList[Int], p: Int => Boolean) =>
    assertEquals(xs.filterNot(p).toList, xs.toList.filterNot(p))
  })
  property("BList collect consistent with List")(forAll { (xs: BList[Int], pf: PartialFunction[Int, Int]) =>
    assertEquals(xs.collect(pf).toList, xs.toList.collect(pf))
  })

  property("iterator works")(forAll { (xs: BList[Int]) =>
    assertEquals(BList.fromList(xs.iterator.toList), xs)
  })
  property("BList constructor can be iterated over as expected")(forAll { (xs: List[Int], p: Int => Boolean) =>
    val l1 = BList.from(xs)
    var l2: BList[Int] = BList.empty
    for (elmt <- xs.reverseIterator) {
      l2 = l2.prepend(elmt)
    }
    assertEquals(l1.filter(p), l2.filter(p)) // iteration over both might expose more errors...
  })
  
  property("BList.traverseVoid/traverse consistency")(forAll { (xs: BList[Int], fn: Int => Option[String]) =>
    assertEquals(xs.traverse(fn).void, xs.traverseVoid(fn))
  })
  property("BList.nonEmptyTraverseVoid/traverseVoid/nonEmptyTraverse consistency")(forAll { (xs:BList.NonEmpty[Int], fn: Int => Option[String]) =>
    assertEquals(xs.traverseVoid(fn), xs.nonEmptyTraverseVoid(fn))
    assertEquals(xs.traverseVoid(fn), xs.nonEmptyTraverse(fn).void)
  })

  property("traverseVoid matches standard traverse baseline")(forAll { (bList: BList[Int], f: Int => Option[String]) =>
    assertEquals(bList.traverse(f).map(_ => ()),bList.traverseVoid(f))
  })

  // property("BList.nonEmptyTraverseVoid/traverseVoid/nonEmptyTraverse consistency and order")(
  //    forAll { (xs: BList.NonEmpty[Int]) =>
  //      val fn = (x: Int) => State.modify[List[Int]](log => log :+ x)
  //     val (logFromListTraverse, _)       = xs.toList.traverse(fn).void.run(Nil).value
      //  val (logFromTraverseVoid, _)       = xs.traverseVoid(fn).run(Nil).value
      //  val (logFromNonEmptyTraverseVoid, _) = xs.nonEmptyTraverseVoid(fn).run(Nil).value
  //     val (logFromNonEmptyTraverse, _)     = xs.nonEmptyTraverse(fn).void.run(Nil).value

      // println(xs.toStringInBlocks)
      // println(logFromListTraverse)
      // println(logFromTraverseVoid)
      // println(logFromNonEmptyTraverseVoid)
      // println(logFromNonEmptyTraverse)
      //assertEquals(logFromTraverseVoid, logFromNonEmptyTraverseVoid)
      //assertEquals(logFromTraverseVoid, logFromNonEmptyTraverse)
  //    }
  //  )

  
  // property("generator")(forAll { (xs: BList[Int]) =>
  //   println(xs.size)
  //   println(xs.toStringInBlocks)
  // })

}
