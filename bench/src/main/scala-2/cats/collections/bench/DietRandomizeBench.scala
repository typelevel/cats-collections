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

import scala.util.Random

/**
 * In reality, no one uses the best and worst scenario, so this is a complete randomized benchmark
 */
@State(Scope.Benchmark)
class DietRandomizeBench extends BigNumberLists {

//  import dogs.Predef._

  implicit val scalazEnumInt: Monoid[Int] with Enum[Int] with Show[Int] = scalaz.std.anyVal.intInstance

  @Benchmark
  def dogsDietAddRandom(): Unit = {
    Random.shuffle(scala).foldLeft(Diet.empty[Int])((d, r) => d + r)
  }

  @Benchmark
  def scalazDievAddRandom(): Unit = {
    Random.shuffle(scalazlst.toList).foldLeft(Diev.empty[Int])((d, r) => d + r)
  }

  @Benchmark
  def dogsDietAddRangeRandom(): Unit = {
    Random.shuffle(scala).foldLeft(Diet.empty[Int])((d, r) => d + Range(r, r + 10))
  }

  @Benchmark
  def scalazDievAddRangeRandom(): Unit = {
    var diev = Diev.empty[Int]

    Random.shuffle(scalazlst.toList).foldLeft(Diev.empty[Int])((d, r) => d + ((r, r + 10)))
  }
}
