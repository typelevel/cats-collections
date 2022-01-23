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

import org.openjdk.jmh.annotations.{Benchmark, Scope, Setup, State}

import scala.util.Random
import scalaz.{Diev, Enum, Monoid, Show}
import cats._

@State(Scope.Benchmark)
class DietBenchSearch {

  implicit val scalazEnumInt: Monoid[Int] with Enum[Int] with Show[Int] = scalaz.std.anyVal.intInstance

  var diet = Diet.empty[Int]
  var diev = Diev.empty[Int]

  @Setup
  def setup: Unit = {
    var i = 0
    while (i < 1000) {
      val s = Random.nextInt()
      val e = s + Random.nextInt()

      diet = diet + Range(s, e)
      diev = diev + (s, e)

      i = i + 1
    }
  }

  @Benchmark
  def dogsDietSearch: Unit = {
    scala.Range(0, 1000).foreach(i => diet.contains(i))
  }

  @Benchmark
  def scalazDievSearch: Unit = {
    scala.Range(0, 1000).foreach(i => diev.contains(i))
  }
}
