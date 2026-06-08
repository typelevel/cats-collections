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

import munit.DisciplineSuite
import org.scalacheck.Test
import scala.math.pow

object global {
  val BlockSize: Int = 4
}

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

    assertEquals(n1.toString, n2.toString)
  }

  test("uncons when tail is a new node") {
    val l = BList.empty[Int].prepend(4).prepend(3).prepend(2).prepend(1)
    val m = BList.empty[Int].prepend(0)

    val n = m.concat(l)

    n.uncons match {
      case None         => fail("should not be empty")
      case Some((h, t)) =>
        assertEquals(h, m.head)
        assertEquals(t.toString, l.toString)
    }
  }

  test("prepend when block is full") {
    var l = BList.empty[Int]
    for (i <- (0 until global.BlockSize).reverse) {
      l = l.prepend(i)
    }
    val m = l.prepend(-1)
    assertEquals(m.toList, (-1 until global.BlockSize).toList)
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
    for (i <- (0 until global.BlockSize).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    assertEquals(m.getUnsafe((global.BlockSize + (global.BlockSize - 1)).toLong), 2)
    assertEquals(m.get((global.BlockSize + (global.BlockSize - 1)).toLong), Some(2))
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
    for (i <- (0 until global.BlockSize).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    // size is triangle numbers in blocksize. size should be  n(n+1)/2 where n is blocksize
    // so blocksize ** 2 + 1 will always be out of bounds
    val idx: Long = pow(global.BlockSize.toDouble, 2.0).toLong + 1L
    assertEquals(m.get(idx), None)
    intercept[IndexOutOfBoundsException](m.getUnsafe(idx))
  }
  test("lastOption with incomplete blocks") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until global.BlockSize).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }

    // last should be blocksize-1
    assertEquals(m.lastOption, Some(global.BlockSize - 1))
  }
  test("lastOption on empty list") {
    val l = BList.empty[Int]
    assertEquals(l.lastOption, None)
  }
  test("size on a triangle number construction") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until global.BlockSize).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }
    // size is triangle numbers in blocksize. formula: n(n+1)/2
    val expected: Long = (global.BlockSize * (global.BlockSize + 1)).toLong / 2L
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
      case None         => fail("should not be empty")
      case Some((h, t)) =>
        assertEquals(h, 0)
        assertEquals(t.toString, l.toString)
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
    assertEquals(m.drop(10).toString, BList.empty[Int].toString)
    assertEquals(m.drop(20).toString, BList.empty[Int].toString)
    assertEquals(m.drop(1000).toString, BList.empty[Int].toString)
  }
  test("drop nothing") {
    var l = BList.empty[Int]
    var m = BList.empty[Int]
    for (i <- (0 until 4).reverse) {
      l = l.prepend(i)
      m = l.concat(m)
    }

    assertEquals(m.drop(0).toString, m.toString)
    assertEquals(m.drop(-10).toString, m.toString)
    assertEquals(m.drop(-200).toString, m.toString)
  }
}
