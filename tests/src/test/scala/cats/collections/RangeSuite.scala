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

import cats.Order
import cats.Show
import cats.syntax.order._
import cats.syntax.show._
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Prop._

class RangeSuite extends ScalaCheckSuite {
  import RangeSuite._

  property("contains(x): returns true for x within [start, end]") {
    val gen = for {
      range <- genAscRangeX
      x <- genX(range)
    } yield (range, x)

    forAll(gen) { case (range: Range[X], x: X) =>
      assert(range.contains(x))
    }
  }
  property("contains(x): returns false for x < start") {
    val gen = for {
      range <- genAscRangeX(minX.succ, maxX)
      x <- genX(minX, range.start.pred)
    } yield (range, x)

    forAll(gen) { case (range: Range[X], x: X) =>
      assert(!range.contains(x))
    }
  }
  property("contains(x): returns false for x > end") {
    val gen = for {
      range <- genAscRangeX(minX, maxX.pred)
      x <- genX(range.end.succ, maxX)
    } yield (range, x)

    forAll(gen) { case (range: Range[X], x: X) =>
      assert(!range.contains(x))
    }
  }
  property("contains(Range): returns true if the passed range is within this range") {
    val gen = for {
      range1 <- genAscRangeX
      range2 <- genAscRangeX(range1)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assert(range1.contains(range2))
    }
  }
  property("contains(Range): returns false if the passed range's start < this range's start") {
    val gen = for {
      range1 <- genAscRangeX(minX.succ, maxX)
      range2 <- genAscRangeX(minX, range1.start.pred, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assert(!range1.contains(range2))
    }
  }
  property("contains(Range): returns false if the passed range's end > this range's end") {
    val gen = for {
      range1 <- genAscRangeX(minX, maxX.pred)
      range2 <- genAscRangeX(minX, range1.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assert(!range1.contains(range2))
    }
  }
  property("contains(Range): returns false if the passed range is a super-range of this one") {
    val gen = for {
      range1 <- genAscRangeX(minX.succ, maxX.pred)
      range2 <- genAscRangeX(minX, range1.start.pred, range1.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assert(!range1.contains(range2))
    }
  }

  property(
    "overlaps(Range): returns true if the start of the first range is contained in the passed range"
  ) {
    val gen = for {
      range1 <- genAscRangeX(minX.succ, maxX.pred)
      range2 <- genAscRangeX(minX, minX, range1.start.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assert(range1.overlaps(range2))
      assert(range2.overlaps(range1))
    }
  }

  property(
    "overlaps(Range): returns true if the end of the first range is contained in the passed range"
  ) {
    val gen = for {
      range1 <- genAscRangeX(minX.succ, maxX.pred)
      range2 <- genAscRangeX(minX, range1.end.pred, maxX, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assert(range1.overlaps(range2))
      assert(range2.overlaps(range1))
    }
  }

  property(
    "overlaps(Range): returns true if the passed range is a super-range of the first range"
  ) {
    val gen = for {
      range1 <- genAscRangeX(minX.succ, maxX.pred)
      range2 <- genAscRangeX(minX, range1.start.pred, range1.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assert(range1.overlaps(range2))
      assert(range2.overlaps(range1))
    }
  }

  property("overlaps(Range): returns false if the first range stops before the passed range starts") {
    val gen = for {
      range1 <- genAscRangeX(minX, maxX.pred)
      range2 <- genAscRangeX(range1.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assert(!range1.overlaps(range2))
      assert(!range2.overlaps(range1))
    }
  }

  property("overlaps(Range): behavior should be consistent with & method") {
    val genOverlapping = for {
      range1 <- genAscRangeX(minX.succ, maxX.pred)
      range2 <- genAscRangeX(minX, range1.end.pred, maxX, maxX)
    } yield (range1, range2)

    val genNonOverlapping = for {
      range1 <- genAscRangeX(minX, maxX.pred)
      range2 <- genAscRangeX(range1.end.succ, maxX)
    } yield (range1, range2)

    forAll(genOverlapping) { case (range1: Range[X], range2: Range[X]) =>
      assert((range1 & range2).isDefined && range1.overlaps(range2))
    }

    forAll(genNonOverlapping) { case (range1: Range[X], range2: Range[X]) =>
      assert((range1 & range2).isEmpty && !range1.overlaps(range2))
    }
  }

  property("foreach: enumerates all values of a range") {
    // Executes a passed `foreach` function and calculates some value that can be used for comparison.
    def calc[A](f: A => Int, foreach: (A => Unit) => Unit) = {
      val bldr = Vector.newBuilder[Int]

      foreach { b =>
        bldr += f(b)
      }

      bldr.result()
    }

    forAll { (range: Range[X]) =>
      val expected = calc[Int](identity, mkScalaRange(range).foreach)
      val obtained = calc[X](_.x, range.foreach)
      assertEquals(obtained, expected)
    }
  }

  property("map: returns a mapped range") {
    forAll { (range: Range[String], f: String => X) =>
      val expected = Range(f(range.start), f(range.end))
      val obtained = range.map(f)

      assertEquals(obtained, expected)
    }
  }

  property("foldLeft: folds range values from start to end") {
    forAll { (range: Range[X], s: String, f: (String, X) => String) =>
      val expected = mkScalaRange(range).foldLeft(s) { (ss, x) => f(ss, X(x)) }
      val obtained = range.foldLeft(s, f)
      assertEquals(obtained, expected)
    }
  }
  property("foldRight: folds range values from end to start") {
    forAll { (range: Range[X], s: String, f: (X, String) => String) =>
      val expected = mkScalaRange(range).foldRight(s) { (x, ss) => f(X(x), ss) }
      val obtained = range.foldRight(s, f)
      assertEquals(obtained, expected)
    }
  }

  property("diff(-): returns None if the passed range completely covers this one") {
    val gen = for {
      range1 <- genAscRangeX
      range2 <- genAscRangeX(range1)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assertEquals(range2 - range1, None)
    }
  }
  property("diff(-): returns this range if it does not overlap the passed one") {
    val gen = for {
      range1 <- genAscRangeX(minX, maxX.pred)
      range2 <- genAscRangeX(range1.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assertEquals(range1 - range2, Some((range1, None)))
      assertEquals(range2 - range1, Some((range2, None)))
    }
  }
  property("diff(-): returns a single range if the passed range partially overlaps this one on the left") {
    val gen = for {
      range2 <- genAscRangeX(minX, maxX.pred)
      range1 <- genAscRangeX(range2.start, range2.end, range2.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      val obtained = range1 - range2
      val expected = Some((Range(range2.end.succ, range1.end), None))
      assertEquals(obtained, expected)
    }
  }
  property("diff(-): returns a single range if the passed range partially overlaps this one on the right") {
    val gen = for {
      range2 <- genAscRangeX(minX.succ, maxX)
      range1 <- genAscRangeX(minX, range2.start.pred, range2.start, range2.end)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      val obtained = range1 - range2
      val expected = Some((Range(range1.start, X(range2.start.x - 1)), None))
      assertEquals(obtained, expected)
    }
  }
  property("diff(-): returns two ranges if the passed range fully fits in the middle of this one") {
    val gen = for {
      range2 <- genAscRangeX(minX.succ, maxX.pred)
      range1 <- genAscRangeX(minX, range2.start.pred, range2.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      val obtained = range1 - range2
      val expected = Some((Range(range1.start, range2.start.pred), Some(Range(range2.end.succ, range1.end))))
      assertEquals(obtained, expected)
    }
  }

  property("add(+): returns a single range if one of the ranges is a subrange of the other") {
    val gen = for {
      range1 <- genAscRangeX
      range2 <- genAscRangeX(range1)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      val expected = (Range(range1.start, range1.end), None)
      val obtained1 = range1 + range2
      val obtained2 = range2 + range1

      assertEquals(obtained1, expected)
      assertEquals(obtained2, expected)
    }
  }
  property("add(+): returns a single range if two passed ranges are connected or patrially overlapping") {
    val gen = for {
      range1 <- genAscRangeX(minX, maxX.pred)
      range2 <- genAscRangeX(range1.start, range1.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      val expected = (Range(range1.start, range2.end), None)
      val obtained1 = range1 + range2
      val obtained2 = range2 + range1

      assertEquals(obtained1, expected)
      assertEquals(obtained2, expected)
    }
  }
  property("add(+): returns both passed ranges if there is a gap between them") {
    val gen = for {
      range1 <- genAscRangeX(minX, maxX.pred.pred) // two pred-s ensure the gap
      range2 <- genAscRangeX(range1.end.succ.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      val obtained1 = range1 + range2
      val obtained2 = range2 + range1

      assertEquals(obtained1, (range1, Some(range2)))
      assertEquals(obtained2, (range1, Some(range2)))
    }
  }

  property("intersect(&): returns a smaller range if one of the ranges is subrange of the other") {
    val gen = for {
      range1 <- genAscRangeX
      range2 <- genAscRangeX(range1)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      val obtained1 = range1 & range2
      val obtained2 = range2 & range1

      assertEquals(obtained1, Some(range2))
      assertEquals(obtained2, Some(range2))
    }
  }
  property("intersect(&): returns None for non-overlapping ranged") {
    val gen = for {
      range1 <- genAscRangeX(minX, maxX.pred)
      range2 <- genAscRangeX(range1.end.succ, maxX)
    } yield (range1, range2)

    forAll(gen) { case (range1: Range[X], range2: Range[X]) =>
      assertEquals(range1 & range2, None)
      assertEquals(range2 & range1, None)
    }
  }

  property("toInterator/toList: converts a range to Iterator/List") {
    forAll { (range: Range[X]) =>
      val expected = {
        val step = if (range.start < range.end) 1 else -1
        List.range(range.start.x, range.end.x, step).map(X(_)) :+ range.end
      }
      // Check both `toIterator` and `toList`.
      assertEquals(range.toIterator.toList, expected)
      assertEquals(range.toList, expected)
    }
  }

  property("reverse: inverses a range") {
    forAll { (range: Range[X]) =>
      val expected = Range(range.end, range.start)
      val obtained = range.reverse
      assertEquals(obtained, expected)
    }
  }

  property("show: renders a math representation of a range") {
    forAll { (range: Range[X]) =>
      val expected = s"[<${range.start.x}>, <${range.end.x}>]"
      val obtained = range.show
      assertEquals(obtained, expected)
    }
  }
}

object RangeSuite {
  // TODO: this has to be placed in cats-collections-scalacheck
  implicit def arbRange[XX](implicit arbX: Arbitrary[XX]): Arbitrary[Range[XX]] =
    Arbitrary(
      Arbitrary.arbitrary[(XX, XX)].map(Function.tupled(Range[XX]))
    )

  final private case class X(x: Int) {
    import X._
    // Fails tests early if x exceed the allowed range.
    require(
      x >= minVal && x <= maxVal,
      s"x must be in range [$minVal, $maxVal] but got $x"
    )

    def succ: X = X(x + 1)
    def pred: X = X(x - 1)
  }
  private object X {
    final val minVal = 0
    final val maxVal = 99
  }
  private val minX = X(X.minVal)
  private val maxX = X(X.maxVal)

  private def genX(min: X, max: X): Gen[X] = Gen.chooseNum(min.x, max.x).map(X(_))
  private def genX(range: Range[X]): Gen[X] = genX(range.start, range.end)
  private val genX: Gen[X] = genX(minX, maxX)

  implicit private val scalaOrdX: Ordering[X] = Ordering.by(_.x)
  implicit private val catsOrdX: Order[X] = Order.fromOrdering[X]
  implicit private val catsShowX: Show[X] = Show.show(x => s"<${x.x}>")

  implicit private val schkArbX: Arbitrary[X] = Arbitrary(genX)
  implicit private val schkCogX: Cogen[X] = Cogen[Int].contramap(_.x)

  implicit private val catsDiscrX: Discrete[X] = new Discrete[X] {
    override def succ(x: X): X = x.succ
    override def pred(x: X): X = x.pred
  }

  private def genAscRangeX(
    minStart: X,
    maxStart: X,
    minEnd: X,
    maxEnd: X
  ): Gen[Range[X]] = {
    require(minStart >= minX && minStart <= maxX, s"incorrect minStart=$minStart")
    require(maxStart >= minStart && maxStart <= maxX, s"incorrect maxStart=$maxStart (minStart=$minStart)")
    require(minEnd >= minStart && minEnd <= maxX, s"incorrect minEnd=$minEnd (minStart=$minStart)")
    require(maxEnd >= minEnd && maxEnd <= maxX, s"incorrect maxEnd (minEnd=$minEnd)")
    for {
      start <- genX(minStart, maxStart)
      end <- genX(Order.max(minEnd, start), maxEnd)
    } yield {
      assert(start <= end, s"$start is not less or equal to $end")
      Range(start, end)
    }
  }
  private def genAscRangeX(min: X, mid: X, max: X): Gen[Range[X]] =
    genAscRangeX(min, mid, mid, max)
  private def genAscRangeX(min: X, max: X): Gen[Range[X]] =
    genAscRangeX(min, max, min, max)

  private def genAscRangeX(range: Range[X]): Gen[Range[X]] =
    genAscRangeX(range.start, range.end)

  private val genAscRangeX: Gen[Range[X]] =
    genAscRangeX(minX, maxX)

  private def mkScalaRange(range: Range[X]) = range.start.x to range.end.x
}
