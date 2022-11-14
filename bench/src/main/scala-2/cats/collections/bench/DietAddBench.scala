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
package bench

import org.openjdk.jmh.annotations._
import scalaz.Diev
import scalaz.Enum
import scalaz.Monoid
import scalaz.Show

@State(Scope.Thread)
class BestCaseRangesList {
  @Param(Array("10", "100", "1000", "10000"))
  var n: Int = _

  var scalazRanges: IndexedSeq[scala.Range] = _
  var dogRanges: IndexedSeq[Range[Int]] = _

  var scalazValues: IndexedSeq[Int] = _
  var dogValues: IndexedSeq[Int] = _

  def getBestCaseDataScalaz: scala.IndexedSeq[scala.Range] = {
    for (
      x <- scala.Range(1, n)
      if x % 10 == 0
    ) yield scala.Range(x, x + 10)
  }

  def getBestCaseDataDogs: scala.IndexedSeq[Range[Int]] = {
    for (
      x <- scala.Range(1, n)
      if x % 10 == 0
    ) yield Range(x, x + 10)
  }

  @Setup
  def setup(): Unit = {
    scalazRanges = getBestCaseDataScalaz
    dogRanges = getBestCaseDataDogs

    scalazValues = 1 to n
    dogValues = 1 to n
  }
}

@State(Scope.Thread)
class DietAddBench extends BestCaseRangesList {

  implicit val scalazEnumInt: Monoid[Int] with Enum[Int] with Show[Int] = scalaz.std.anyVal.intInstance

  @Benchmark
  def dogsDietAdd(): Unit = {
    var diet = Diet.empty[Int]

    dogValues.foreach { i => diet = diet + i }
  }

  @Benchmark
  def scalazDievAdd(): Unit = {
    scalazValues.foldLeft(Diev.empty[Int])((d, r) => d + r)
  }

  @Benchmark
  def dogsDietAddRange(): Unit = {
    dogRanges.foldLeft(Diet.empty[Int])((d, r) => d + Range(r.start, r.end))
  }

  @Benchmark
  def scalazDievAddRange(): Unit = {
    scalazRanges.foldLeft(Diev.empty[Int])((d, r) => d + ((r.start, r.end)))
  }
}
