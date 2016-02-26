/**
  * Created by anicolaspp on 2/18/16.
  */

package dogs
package bench

import dogs.Predef._
import org.openjdk.jmh.annotations._

import scala.collection.immutable.List
import scala.util.Random
import scalaz.{IList, Diev}

@State(Scope.Thread)
class BestCaseRangesList {
  @Param(Array("10", "100", "1000", "10000"))
  var n: Int = _

  var scalazRanges: IndexedSeq[scala.Range] = _
  var dogRanges: IndexedSeq[Range[Predef.Int]] = _

  var scalazValues: IndexedSeq[Int] = _
  var dogValues: IndexedSeq[Predef.Int] = _

  def getBestCaseDataScalaz: scala.IndexedSeq[scala.Range] = {
    for (x <- scala.Range(1, n)
         if (x % 10 == 0)
    ) yield scala.Range(x, x + 10)
  }

  def getBestCaseDataDogs: scala.IndexedSeq[Range[Predef.Int]] = {
    for (x <- scala.Range(1, n)
         if (x % 10 == 0)
    ) yield Range(x, x + 10)
  }

  @Setup
  def setup: Unit = {
    scalazRanges = getBestCaseDataScalaz
    dogRanges = getBestCaseDataDogs

    scalazValues = (1 to n)
    dogValues = (1 to n)
  }
}

@State(Scope.Thread)
class DietAddBench extends BestCaseRangesList {

  implicit val scalazEnumInt = scalaz.std.anyVal.intInstance

  @Benchmark
  def dogsDietAdd: Unit = {
    var diet = dogs.Diet.empty[Int]

    dogValues.foreach{ i => diet = diet + i }
  }

  @Benchmark
  def scalazDievAdd: Unit = {
    scalazValues.foldLeft(Diev.empty[Int])((d, r) => d + r)
  }

  @Benchmark
  def dogsDietAddRange: Unit = {
    dogRanges.foldLeft(Diet.empty[Int])((d, r) => d + Range(r.start, r.end))
  }

  @Benchmark
  def scalazDievAddRange: Unit = {
    scalazRanges.foldLeft(Diev.empty[Int])((d, r) => d + (r.start, r.end))
  }
}




