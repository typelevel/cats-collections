package cats.collections

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import scala.collection.{immutable, mutable}
import scala.math.pow
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class BitSetBenchmarks {

  // ranges from 0.0 to 1.0.
  @Param(Array("0.0625"))
  var density: Double = _

  // creates bitsets of size 2^k (2^10 = 1024, 2^20 = ~1M).
  @Param(Array("14"))
  var exponent: Int = _

  var size: Int = _
  var indices: Array[Int] = _

  var values: Array[Int] = _
  var sci: immutable.BitSet = _
  var scm: mutable.BitSet = _
  var iset: immutable.Set[Int] = _
  var ccbs: _root_.cats.collections.BitSet = _

  var values2: Array[Int] = _
  var sci2: immutable.BitSet = _
  var scm2: mutable.BitSet = _
  var iset2: immutable.Set[Int] = _
  var ccbs2: _root_.cats.collections.BitSet = _

  @Setup
  def setup(): Unit = {
    size = pow(2, exponent.toDouble).toInt

    val r = new Random(0x13572468)

    values = (0 until size).iterator.filter { _ =>
      r.nextDouble < density
    }.toArray

    sci = immutable.BitSet(values: _*)
    scm = mutable.BitSet(values: _*)
    iset = immutable.Set(values: _*)
    ccbs = _root_.cats.collections.BitSet(values: _*)

    values2 = (0 until size).iterator.filter { _ =>
      r.nextDouble < density
    }.toArray

    sci2 = immutable.BitSet(values2: _*)
    scm2 = mutable.BitSet(values2: _*)
    iset2 = immutable.Set(values2: _*)
    ccbs2 = _root_.cats.collections.BitSet(values2: _*)

    val n = size / 20
    indices = new Array[Int](n)
    (0 until n).foreach { i =>
      indices(i) = r.nextInt(size)
    }
  }

  // @Benchmark
  // def buildSci(): immutable.BitSet =
  //   immutable.BitSet(values: _*)

  // @Benchmark
  // def buildScm(): mutable.BitSet =
  //   mutable.BitSet(values: _*)

  // @Benchmark
  // def buildIset(): immutable.Set[Int] =
  //   immutable.Set[Int](values: _*)

  @Benchmark
  def buildCcbs(): _root_.cats.collections.BitSet =
    _root_.cats.collections.BitSet(values: _*)

  // @Benchmark
  // def foldIntoSci(): immutable.BitSet =
  //   values.foldLeft(immutable.BitSet.empty)(_ + _)

  // @Benchmark
  // def foldIntoScm(): mutable.BitSet = {
  //   val b = mutable.BitSet.empty
  //   values.foreach(b += _)
  //   b
  // }

  // @Benchmark
  // def foldIntoIset(): immutable.Set[Int] =
  //   values.foldLeft(immutable.Set.empty[Int])(_ + _)

  @Benchmark
  def foldIntoCcbs(): _root_.cats.collections.BitSet =
    values.foldLeft(_root_.cats.collections.BitSet.empty)(_ + _)

  // @Benchmark
  // def lookupSci(): Int = indices.count(i => sci(i))

  // @Benchmark
  // def lookupScm(): Int = indices.count(i => scm(i))

  // @Benchmark
  // def lookupIset(): Int = indices.count(i => iset(i))

  @Benchmark
  def lookupCcbs(): Int = indices.count(i => ccbs(i))

  // @Benchmark
  // def mergeSci(): immutable.BitSet = sci | sci2

  // @Benchmark
  // def mergeScm(): mutable.BitSet = {
  //   val x = scm.clone
  //   x |= scm2
  //   x
  // }

  // @Benchmark
  // def mergeIset(): immutable.Set[Int] = iset | iset2

  @Benchmark
  def mergeCcbs(): _root_.cats.collections.BitSet = ccbs | ccbs2
}
