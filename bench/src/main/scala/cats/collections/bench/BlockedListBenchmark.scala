package cats.collections.bench

import cats.collections.{BlockedList, BlockedListCopy}
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class BlockedListBenchmark {

  /** Block size under test. Stored per-node in both implementations. */
  @Param(Array("4", "8", "16", "32", "64"))
  var blockSize: Int = _

  var preparedList: BlockedList[Int] = _
  var preparedListCopy: BlockedListCopy[Int] = _
  var preparedScalaList: List[Int] = _

  /** Number of elements used in all benchmarks. */
  final val ListSize = 10000

  @Setup(Level.Trial)
  def setup(): Unit = {
    preparedList = BlockedList[Int](List.range(1, 10000))(blockSize)
    preparedListCopy = BlockedListCopy[Int](List.range(1, 10000))(blockSize)
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
    var result = preparedList.uncons
    while (result.isDefined) {
      result = result.get._2.uncons
    }
  }

  @Benchmark
  def copyOnWriteForEach(): Long = {
    var sum = 0L
    preparedList.forEach((a: Int) => sum += a)
    sum
  }

  @Benchmark
  def copyOnWriteFoldLeft(): Long = {
    preparedList.foldLeft(0L)((acc, elem) => acc + elem)

  }

  //  //////////////////////////////////////////////////////////////////////////////////////////////
  // //////////////////////// no copy ////////////////////////
  @Benchmark
  def noCopyPrepend(): BlockedListCopy[Int] = {
    var list = BlockedListCopy.empty[Int](blockSize)
    var i = 1
    while (i <= ListSize) {
      list = list.prepend(i)
      i += 1
    }
    list
  }

  @Benchmark
  def noCopyWriteUncons(): Unit = {
    var result = preparedListCopy.uncons
    while (result.isDefined) {
      result = result.get._2.uncons
    }
  }

  @Benchmark
  def noCopyWriteForEach(): Long = {
    var sum = 0L
    preparedListCopy.forEach((a: Int) => sum += a)
    sum
  }

  @Benchmark
  def noCopyFoldLeft(): Long = {
    preparedListCopy.foldLeft(0L)((acc, elem: Int) => acc + elem)
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