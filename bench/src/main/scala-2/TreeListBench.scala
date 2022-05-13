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

@State(Scope.Thread)
class TreeListBench {
  @Param(Array("100", "1000", "10000"))
  var n: Int = _

  var treeList: TreeList[Int] = _
  var list: List[Int] = _
  var vect: Vector[Int] = _

  @Setup
  def setup: Unit = {
    list = (0 until n).toList
    treeList = TreeList.fromList(list)
    vect = list.toVector
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
  def sumTreeList(bh: Blackhole): Unit = {

    @tailrec
    def loop(ls: TreeList[Int], acc: Int): Int =
      ls.uncons match {
        case None          => acc
        case Some(h, tail) => loop(tail, acc + h)
      }

    bh.consume(loop(treeList, 0))
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
  def randomAccessTreeList(bh: Blackhole): Unit = {

    val rand = new java.util.Random(42)
    @tailrec
    def loop(cnt: Int, acc: Int): Int = {
      val v = treeList.getUnsafe((rand.nextInt() & Int.MaxValue) % n) + acc
      if (cnt <= 0) v
      else loop(cnt - 1, v)
    }

    bh.consume(loop(100, 0))
  }
}
