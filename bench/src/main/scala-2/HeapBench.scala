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

import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, Setup, State}
import org.openjdk.jmh.infra.Blackhole
import scala.util.Random
import cats._

import scala.annotation.tailrec

/**
 * run using, e.g. bench/jmh:run -i 3 -wi 3 -f1 -t1 .*HeapBench.*
 */
@State(Scope.Thread)
class HeapBench {
  @Param(Array("100", "500", "1000", "10000"))
  var n: Int = _

  var data: Array[Int] = _
  var heap: Heap[Int] = _
  var pheap: PairingHeap[Int] = _

  @Setup
  def setup: Unit = {
    val rng = new Random(n)
    data = (0 until n).iterator.map(_ => rng.nextInt()).toArray
    heap = data.foldLeft(Heap.empty[Int])(_.add(_))
    pheap = data.foldLeft(PairingHeap.empty[Int])(_.add(_))
  }

  @Benchmark
  def addHeap(bh: Blackhole): Unit = {
    var i = 0
    var h = Heap.empty[Int]
    while (i < n) {
      h += data(i)
      i += 1
    }
    bh.consume(h)
  }

  @Benchmark
  def addPairingHeap(bh: Blackhole): Unit = {
    var i = 0
    var h = PairingHeap.empty[Int]
    while (i < n) {
      h += data(i)
      i += 1
    }
    bh.consume(h)
  }

  @Benchmark
  def removeAllHeap(bh: Blackhole): Unit = {
    var h = heap
    while (h.nonEmpty) {
      h = h.remove
    }
    bh.consume(h)
  }

  @Benchmark
  def removeAllPairingHeap(bh: Blackhole): Unit = {
    var h = pheap
    while (h.nonEmpty) {
      h = h.remove
    }
    bh.consume(h)
  }

  @Benchmark
  def takeLargestHeap(bh: Blackhole): Unit = {
    bh.consume(Heap.takeLargest(data, n / 10).toList)
  }

  @Benchmark
  def takeLargestPairingHeap(bh: Blackhole): Unit = {
    bh.consume(PairingHeap.takeLargest(data, n / 10).toList)
  }

}
