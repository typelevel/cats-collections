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
import scala.util.Random

@State(Scope.Thread)
class BListBench {
  @Param(Array("100", "1000", "10000"))
  var n: Int = _

  val arr: Array[String] = Array("hello",
                                 "hi",
                                 "corvid",
                                 "purple",
                                 "navel",
                                 "tv",
                                 "32",
                                 "beach",
                                 "rocks",
                                 "town",
                                 "coast",
                                 "tattoo",
                                 "vacuum",
                                 "bug",
                                 "big",
                                 "1604932960",
                                 "mine",
                                 "L",
                                 "blist",
                                 "end"
  )
  var bList: BList[String] = _
  var list: List[String] = _
  var vector: Vector[String] = _

  @Setup
  def setup(): Unit = {
    list = List.fill(n)(arr(Random.nextInt(20)))
    bList = BList.fromList(list)
    vector = list.toVector
  }

  // map
  @Benchmark
  def map_B_List(bh: Blackhole): Unit = {
    bh.consume(bList.map(x => x + "3"))
  }
  @Benchmark
  def map_List(bh: Blackhole): Unit = {
    bh.consume(list.map(x => x + "3"))
  }
  @Benchmark
  def map_Vector(bh: Blackhole): Unit = {
    bh.consume(vector.map(x => x + "3"))
  }

  // concat
  @Benchmark
  def concat_B_List(bh: Blackhole): Unit = {
    var xs = bList
    var i = 1
    while (i < 10) {
      xs = xs ++ bList.take(n / i)
      i += 1
    }
    bh.consume(xs)
  }
  @Benchmark
  def concat_List(bh: Blackhole): Unit = {
    var xs = list
    var i = 1
    while (i < 10) {
      xs = xs ++ list.take(n / i)
      i += 1
    }
    bh.consume(xs)
  }
  @Benchmark
  def concat_Vector(bh: Blackhole): Unit = {
    var xs = vector
    var i = 1
    while (i < 10) {
      xs = xs ++ vector.take(n / i)
      i += 1
    }
    bh.consume(xs)
  }

  // reverse
  @Benchmark
  def reverse_B_List(bh: Blackhole): Unit = {
    bh.consume(bList.reverse)
  }
  @Benchmark
  def reverse_List(bh: Blackhole): Unit = {
    bh.consume(list.reverse)
  }
  @Benchmark
  def reverse_Vector(bh: Blackhole): Unit = {
    bh.consume(vector.reverse)
  }

  // random access
  @Benchmark
  def random_access_B_List(bh: Blackhole): Unit = {
    var x = 0
    var i = 0
    while (i < 100) {
      x = Random.nextInt(n)
      bh.consume(bList.get(x.toLong))
      i += 1
    }
  }
  @Benchmark
  def random_access_List(bh: Blackhole): Unit = {
    var x = 0
    var i = 0
    while (i < 100) {
      x = Random.nextInt(n)
      bh.consume(list(x))
      i += 1
    }
  }
  @Benchmark
  def random_access_Vector(bh: Blackhole): Unit = {
    var x = 0
    var i = 0
    while (i < 100) {
      x = Random.nextInt(n)
      bh.consume(vector(x))
      i += 1
    }
  }

  // take and drop
  @Benchmark
  def takedrop_B_List(bh: Blackhole): Unit = {
    var x = 0
    var i = 0
    while (i < 30) {
      x = Random.nextInt(n)
      bh.consume(bList.take(n))
      bh.consume(bList.drop(n))
      i += 1
    }
    i = 0
    while (i < 10) {
      bh.consume(bList.takeWhile(_.length < 10))
      bh.consume(bList.dropWhile(_.length > 4))
      i += 1
    }
  }
  @Benchmark
  def takedrop_List(bh: Blackhole): Unit = {
    var x = 0
    var i = 0
    while (i < 30) {
      x = Random.nextInt(n)
      bh.consume(list.take(n))
      bh.consume(list.drop(n))
      i += 1
    }
    i = 0
    while (i < 10) {
      bh.consume(list.takeWhile(_.length < 10))
      bh.consume(list.dropWhile(_.length > 4))
      i += 1
    }
  }
  @Benchmark
  def takedrop_Vector(bh: Blackhole): Unit = {
    var x = 0
    var i = 0
    while (i < 30) {
      x = Random.nextInt(n)
      bh.consume(vector.take(n))
      bh.consume(vector.drop(n))
      i += 1
    }
    i = 0
    while (i < 10) {
      bh.consume(vector.takeWhile(_.length < 10))
      bh.consume(vector.dropWhile(_.length > 4))
      i += 1
    }
  }

  // the makeChunk method from fs2 channel (example of real possible usage of blist)
  @Benchmark
  def makeChunkChannel_B_List(bh: Blackhole): Unit = {
    val arr = new Array[Any](n)
    var i = n - 1
    var values = bList
    while (i >= 0) {
      arr(i) = values.headUnsafe
      values = values.tailUnsafe
      i -= 1
    }
    bh.consume(arr)
  }
  @Benchmark
  def makeChunkChannel_List(bh: Blackhole): Unit = {
    val arr = new Array[Any](n)
    var i = n - 1
    var values = list
    while (i >= 0) {
      arr(i) = values.head
      values = values.tail
      i -= 1
    }
    bh.consume(arr)
  }
  @Benchmark
  def makeChunkChannel_Vector(bh: Blackhole): Unit = {
    val arr = new Array[Any](n)
    var i = n - 1
    var values = vector
    while (i >= 0) {
      arr(i) = values.head
      values = values.tail
      i -= 1
    }
    bh.consume(arr)
  }

  // prepending benchmarks
  @Benchmark
  def prepend_BList(bh: Blackhole): Unit = {
    var b: BList[String] = bList
    var i = 0
    while (i < n) {
      b = b.prepend(arr(Random.nextInt(20)))
      i += 1
    }
    bh.consume(b)
  }
  @Benchmark
  def prepend_List(bh: Blackhole): Unit = {
    var b: List[String] = list
    var i = 0
    while (i < n) {
      b = arr(Random.nextInt(20)) :: b
      i += 1
    }
    bh.consume(b)
  }
  @Benchmark
  def prepend_Vector(bh: Blackhole): Unit = {
    var b: Vector[String] = vector
    var i = 0
    while (i < n) {
      b = arr(Random.nextInt(20)) +: b
      i += 1
    }
    bh.consume(b)
  }

  @Benchmark
  def sum_BList(bh: Blackhole): Unit = {
    bh.consume(bList.foldLeft("")((acc, a) => acc + a))
  }
  @Benchmark
  def sum_List(bh: Blackhole): Unit = {
    bh.consume(list.foldLeft("")((acc, a) => acc + a))
  }
  @Benchmark
  def sum_Vector(bh: Blackhole): Unit = {
    bh.consume(vector.foldLeft("")((acc, a) => acc + a))
  }

  // builder AddOne
  @Benchmark
  def addOne_B_List(bh: Blackhole): Unit = {
    val builder = BList.newBuilder[String]
    var i = 0
    while (i < n) {
      builder += arr(Random.nextInt(20))
      i += 1
    }
    bh.consume(builder.result())
  }
  @Benchmark
  def addOne_List(bh: Blackhole): Unit = {
    val builder = List.newBuilder[String]
    var i = 0
    while (i < n) {
      builder += arr(Random.nextInt(20))
      i += 1
    }
    bh.consume(builder.result())
  }
  @Benchmark
  def addOne_Vector(bh: Blackhole): Unit = {
    val builder = Vector.newBuilder[String]
    var i = 0
    while (i < n) {
      builder += arr(Random.nextInt(20))
      i += 1
    }
    bh.consume(builder.result())
  }

  // builder AddAll
  @Benchmark
  def addAll_B_List(bh: Blackhole): Unit = {
    val builder = BList.newBuilder[String]
    builder ++= list
    builder ++= list.take(n / 3)
    builder ++= list.drop(n / 3)
    builder ++= list
    bh.consume(builder.result())
  }
  @Benchmark
  def addAll_List(bh: Blackhole): Unit = {
    val builder = List.newBuilder[String]
    builder ++= list
    builder ++= list.take(n / 3)
    builder ++= list.drop(n / 3)
    builder ++= list
    bh.consume(builder.result())
  }
  @Benchmark
  def addAll_Vector(bh: Blackhole): Unit = {
    val builder = Vector.newBuilder[String]
    builder ++= list
    builder ++= list.take(n / 3)
    builder ++= list.drop(n / 3)
    builder ++= list
    bh.consume(builder.result())
  }

}
