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

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scala.collection.immutable.{HashSet => SHashSet}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class HashSetBench {
  @Param(Array("100", "500", "1000", "10000"))
  var size: Int = _

  var hashSet: HashSet[Long] = _
  var otherHashSet: HashSet[Long] = _
  var scalaSet: SHashSet[Long] = _
  var otherScalaSet: SHashSet[Long] = _
  var pred: Long => Boolean = _

  def hashSetOfSize(n: Int) = HashSet.fromSeq(1L to (n.toLong))
  def scalaSetOfSize(n: Int) = SHashSet.from(1L to (n.toLong))

  @Setup(Level.Trial)
  def init(): Unit = {
    hashSet = hashSetOfSize(size)
    otherHashSet = hashSetOfSize(size)
    scalaSet = scalaSetOfSize(size)
    otherScalaSet = scalaSetOfSize(size)
    pred = (l: Long) => l % 2 == 0
  }

  @Benchmark
  def hashSetFromSeq(bh: Blackhole): Unit =
    bh.consume(hashSetOfSize(size))

  @Benchmark
  def scalaSetFromSeq(bh: Blackhole): Unit =
    bh.consume(scalaSetOfSize(size))

  @Benchmark
  @OperationsPerInvocation(1000)
  def hashSetAdd(bh: Blackhole): Unit = {
    var hs = hashSet
    var i = 0L
    while (i < 1000L) {
      hs = hs.add(-i)
      i += 1L
    }
    bh.consume(hs)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def scalaSetAdd(bh: Blackhole): Unit = {
    var ss = scalaSet
    var i = 0L
    while (i < 1000L) {
      ss = ss.incl(-i)
      i += 1L
    }
    bh.consume(ss)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def hashSetRemove(bh: Blackhole): Unit = {
    var hs = hashSet
    var i = 0L
    while (i < 1000L) {
      hs = hs.remove(i)
      i += 1L
    }
    bh.consume(hs)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def scalaSetRemove(bh: Blackhole): Unit = {
    var ss = scalaSet
    var i = 0L
    while (i < 1000L) {
      ss = ss.excl(i)
      i += 1L
    }
    bh.consume(ss)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def hashSetContains(bh: Blackhole): Unit = {
    var i = 0L
    while (i < 1000L) {
      bh.consume(hashSet.contains(i))
      i += 1L
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def scalaSetContains(bh: Blackhole): Unit = {
    var i = 0L
    while (i < 1000L) {
      bh.consume(scalaSet.contains(i))
      i += 1L
    }
  }

  @Benchmark
  def hashSetForeach(bh: Blackhole): Unit =
    hashSet.foreach(bh.consume)

  @Benchmark
  def scalaSetForeach(bh: Blackhole): Unit =
    scalaSet.foreach(bh.consume)

  @Benchmark
  def hashSetIterator(bh: Blackhole): Unit = {
    val it = hashSet.iterator
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def scalaSetIterator(bh: Blackhole): Unit = {
    val it = scalaSet.iterator
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def hashSetUnion(bh: Blackhole): Unit =
    bh.consume(hashSet.union(otherHashSet))

  @Benchmark
  def scalaSetUnion(bh: Blackhole): Unit =
    bh.consume(scalaSet | otherScalaSet)

  @Benchmark
  def hashSetDiff(bh: Blackhole): Unit =
    bh.consume(hashSet.diff(otherHashSet))

  @Benchmark
  def scalaSetDiff(bh: Blackhole): Unit =
    bh.consume(scalaSet.diff(otherScalaSet))

  @Benchmark
  def hashSetIntersect(bh: Blackhole): Unit =
    bh.consume(hashSet.intersect(otherHashSet))

  @Benchmark
  def scalaSetIntersect(bh: Blackhole): Unit =
    bh.consume(scalaSet & otherScalaSet)

  @Benchmark
  def hashSetFilter(bh: Blackhole): Unit =
    bh.consume(hashSet.filter(pred))

  @Benchmark
  def scalaSetFilter(bh: Blackhole): Unit =
    bh.consume(scalaSet.filter(pred))

  @Benchmark
  def hashSetFilterNot(bh: Blackhole): Unit =
    bh.consume(hashSet.filterNot(pred))

  @Benchmark
  def scalaSetFilterNot(bh: Blackhole): Unit =
    bh.consume(scalaSet.filterNot(pred))

  @Benchmark
  def hashSetUniversalEquals(bh: Blackhole): Unit =
    bh.consume(hashSet == otherHashSet)

  @Benchmark
  def hashSetEqEquals(bh: Blackhole): Unit =
    bh.consume(hashSet === otherHashSet)

  @Benchmark
  def scalaSetUniversalEquals(bh: Blackhole): Unit =
    bh.consume(scalaSet == otherScalaSet)
}
