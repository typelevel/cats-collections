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

package cats.collections.bench

import cats.collections.{BlockedList, FastBlockedList}
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class BlockedListBenchmark {

  /**
   * Block size under test. Stored per-node in both implementations.
   */
  @Param(Array("4", "8", "16", "32", "64"))
  var blockSize: Int = _

  var preparedBlockedList: BlockedList[Int] = _
  var preparedFastBlockedList: FastBlockedList[Int] = _
  var preparedScalaList: List[Int] = _

  /**
   * Number of elements used in all benchmarks.
   */
  final val ListSize = 10000

  @Setup(Level.Trial)
  def setup(): Unit = {
    preparedBlockedList = BlockedList[Int](List.range(1, 10000))(blockSize)
    preparedFastBlockedList = FastBlockedList[Int](List.range(1, 10000))(blockSize)
    preparedScalaList = List.range(1, 10000)

  }

  // ////////////////////////////////copy on write ///////////////////////////////
  @Benchmark
  def copyOnWritePrepend(): BlockedList[Int] = {
    var list = BlockedList.empty[Int](blockSize)
    var i = 1
    while (i <= ListSize) {
      list = list.prepend(i)
      i += 1
    }
    list
  }

  @Benchmark
  def copyOnWriteUncons(): Unit = {
    var result = preparedBlockedList.uncons
    while (result.isDefined) {
      result = result.get._2.uncons
    }
  }

  @Benchmark
  def copyOnWriteForEach(): Long = {
    var sum = 0L
    preparedBlockedList.forEach((a: Int) => sum += a)
    sum
  }

  @Benchmark
  def copyOnWriteFoldLeft(): Long = {
    preparedBlockedList.foldLeft(0L)((acc, elem) => acc + elem)

  }

  //  //////////////////////////////////////////////////////////////////////////////////////////////
  // //////////////////////// no copy ////////////////////////
  @Benchmark
  def noCopyPrepend(): FastBlockedList[Int] = {
    var list = FastBlockedList.empty[Int](blockSize)
    var i = 1
    while (i <= ListSize) {
      list = list.prepend(i)
      i += 1
    }
    list
  }

  @Benchmark
  def noCopyWriteUncons(): Unit = {
    var result = preparedFastBlockedList.uncons
    while (result.isDefined) {
      result = result.get._2.uncons
    }
  }

  @Benchmark
  def noCopyWriteForEach(): Long = {
    var sum = 0L
    preparedFastBlockedList.forEach((a: Int) => sum += a)
    sum
  }

  @Benchmark
  def noCopyFoldLeft(): Long = {
    preparedFastBlockedList.foldLeft(0L)((acc, elem: Int) => acc + elem)
  }
  //  /////////////////////////////////////////////////////////////

  // //////////////////////////// Naive List //////////////////////

  @Benchmark
  def scalaListPrepend(): List[Int] = {
    var list = List.empty[Int]
    var i = 1
    while (i <= ListSize) {
      list = i :: list
      i += 1
    }
    list
  }

  @Benchmark
  def scalaListUncons(): Unit = {
    var list = preparedScalaList
    while (list.nonEmpty) {
      list = list.tail
    }
  }

  @Benchmark
  def scalaListFoldLeft(): Long = {
    preparedScalaList.foldLeft(0L)((acc, a) => acc + a)
  }

  @Benchmark
  def scalaListForeach(): Long = {
    var sum = 0L
    preparedScalaList.foreach(a => sum += a)
    sum
  }

  //  Map

  @Benchmark
  def blockedListMap(): BlockedList[Int] = {
    preparedBlockedList.map(_ + 1)
  }

  @Benchmark
  def scalaListMap(): List[Int] = {
    preparedScalaList.map(_ + 1)
  }

//I
//  @Benchmark
//  def copyPrepend(): BlockedList[Int] = {
//    // This runs in a loop; you want to measure the cost of a single prepend
//    // Usually you'd have a baseline and a measured operation
//    var list = BlockedList.empty[Int]
//    for (i <- 1 to 1000) {
//      list = list.prepend(i)(blockSize)
//    }
//    list
//  }
//
//  @Benchmark
//  def copyUncons(): Unit = {
//    // Build a large list first, then uncons all elements
//    var list = BlockedList[Int](List.range(1, 10000))(blockSize)
//    while (list.uncons(blockSize).isDefined) {
//      list = list.uncons(blockSize).get._2
//    }
  // }

//
//  @Benchmark
//  def scalaListPrepend1000(): List[Int] = {
//    var list = List.empty[Int]
//    var i = 0
//    while (i < 1000) {
//      list = i :: list
//      i += 1
//    }
//    list
//  }
//
//  // Compare with Scala's Vector prepend
//  @Benchmark
//  def scalaVectorPrepend1000(): Vector[Int] = {
//    var vec = Vector.empty[Int]
//    var i = 0
//    while (i < 1000) {
//      vec = i +: vec
//      i += 1
//    }
//    vec
//  }

}
