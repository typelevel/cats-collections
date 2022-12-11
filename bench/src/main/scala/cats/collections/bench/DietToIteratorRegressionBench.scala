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
import scala.util.Random

@State(Scope.Benchmark)
class DietToIteratorRegressionBench {
  final private val randomSeed = 497169389666431579L

  @Param(Array("100", "1000", "10000"))
  var size: Int = _

  @Param(Array("0", "8", "16", "24"))
  var shiftBits: Int = _

  var diet: Diet[Int] = Diet.empty[Int]

  @Setup
  def setup(): Unit = {
    val random = new Random(randomSeed)

    var i = 0
    while (i < size) {
      val s = random.nextInt()
      val e = s + (random.nextInt() >> shiftBits)

      diet += Range(s, e)
      i += 1
    }
  }

  @Benchmark
  def toIterator(): Unit = {
    diet.toIterator.foreach(Function.const(()))
  }
}
