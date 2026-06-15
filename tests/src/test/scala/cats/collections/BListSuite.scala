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

//import cats.syntax.all._
import cats.collections.arbitrary.blist._
//import cats.laws.discipline._
import cats.Eq
import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.scalacheck.Test
//import org.scalacheck.{Arbitrary, Cogen, Gen, Test}
import scala.math.pow

class BListSuite extends DisciplineSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default

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
    println(n1.toStringInBlocks)
    println(n2.toStringInBlocks)
    assertEquals(n1, n2)

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
}
