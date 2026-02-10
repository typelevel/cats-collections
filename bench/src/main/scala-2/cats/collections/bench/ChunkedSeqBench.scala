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
import org.openjdk.jmh.infra.Blackhole

import scala.annotation.tailrec

@State(Scope.Thread)
class ChunkedSeqBench {
  @Param(Array("100", "1000", "10000"))
  var n: Int = _

  var chunkedSeq: ChunkedSeq[Int] = _
  var chunkedSeqFromPrepend: ChunkedSeq[Int] = _
  var list: List[Int] = _
  var vect: Vector[Int] = _

  @Setup
  def setup(): Unit = {
    list = (0 until n).toList
    chunkedSeq = ChunkedSeq.fromList(list)
    vect = list.toVector

    // Build a ChunkedSeq via prepend to test deep-tree performance
    var cs: ChunkedSeq[Int] = ChunkedSeq.empty
    (0 until n).foreach { i => cs = i :: cs }
    chunkedSeqFromPrepend = cs
  }

  @Benchmark
  def sumList(bh: Blackhole): Unit = {
    @tailrec
    def loop(ls: List[Int], acc: Int): Int =
      ls match {
        case Nil       => acc
        case h :: tail => loop(tail, acc + h)
      }
    bh.consume(loop(list, 0))
  }

  @Benchmark
  def sumVector(bh: Blackhole): Unit = {
    @tailrec
    def loop(ls: Vector[Int], acc: Int): Int =
      if (ls.isEmpty) acc
      else loop(ls.init, ls.last + acc)
    bh.consume(loop(vect, 0))
  }

  @Benchmark
  def sumChunkedSeq(bh: Blackhole): Unit = {
    bh.consume(chunkedSeq.foldLeft(0)(_ + _))
  }

  @Benchmark
  def sumChunkedSeqFromPrepend(bh: Blackhole): Unit = {
    bh.consume(chunkedSeqFromPrepend.foldLeft(0)(_ + _))
  }

  @Benchmark
  def randomAccessList(bh: Blackhole): Unit = {
    val rand = new java.util.Random(42)
    @tailrec
    def loop(cnt: Int, acc: Int): Int = {
      val v = list((rand.nextInt() & Int.MaxValue) % n) + acc
      if (cnt <= 0) v
      else loop(cnt - 1, v)
    }
    bh.consume(loop(100, 0))
  }

  @Benchmark
  def randomAccessVector(bh: Blackhole): Unit = {
    val rand = new java.util.Random(42)
    @tailrec
    def loop(cnt: Int, acc: Int): Int = {
      val v = vect((rand.nextInt() & Int.MaxValue) % n) + acc
      if (cnt <= 0) v
      else loop(cnt - 1, v)
    }
    bh.consume(loop(100, 0))
  }

  @Benchmark
  def randomAccessChunkedSeq(bh: Blackhole): Unit = {
    val rand = new java.util.Random(42)
    @tailrec
    def loop(cnt: Int, acc: Int): Int = {
      val v = chunkedSeq.getUnsafe((rand.nextInt() & Int.MaxValue).toLong % n) + acc
      if (cnt <= 0) v
      else loop(cnt - 1, v)
    }
    bh.consume(loop(100, 0))
  }

  @Benchmark
  def prependList(bh: Blackhole): Unit = {
    var ls: List[Int] = Nil
    var i = 0
    while (i < n) { ls = i :: ls; i += 1 }
    bh.consume(ls)
  }

  @Benchmark
  def prependChunkedSeq(bh: Blackhole): Unit = {
    var cs: ChunkedSeq[Int] = ChunkedSeq.empty
    var i = 0
    while (i < n) { cs = i :: cs; i += 1 }
    bh.consume(cs)
  }

  @Benchmark
  def appendVector(bh: Blackhole): Unit = {
    var v: Vector[Int] = Vector.empty
    var i = 0
    while (i < n) { v = v :+ i; i += 1 }
    bh.consume(v)
  }

  @Benchmark
  def appendChunkedSeq(bh: Blackhole): Unit = {
    var cs: ChunkedSeq[Int] = ChunkedSeq.empty
    var i = 0
    while (i < n) { cs = cs :+ i; i += 1 }
    bh.consume(cs)
  }
}
