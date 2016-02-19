/**
  * Created by anicolaspp on 2/18/16.
  */

package dogs
package bench

import dogs.Predef._
import org.openjdk.jmh.annotations.{Benchmark, Scope, Setup, State}

import scala.util.Random
import scalaz.Diev

@State(Scope.Benchmark)
class DietAddBench {

  implicit object scalazEnumInt extends scalaz.Enum[Int] {
    override def succ(a: Int): Int = a + 1

    override def pred(a: Int): Int = a - 1

    override def order(x: Int, y: Int): scalaz.Ordering = {
      if (x == y) scalaz.Ordering.EQ
      else if (x < y) scalaz.Ordering.LT
      else scalaz.Ordering.GT
    }
  }

  var items: Seq[Int] = Seq[Int]()
  var ranges = Seq[scala.Range]()
  var toSearch = Seq[Int]()

  @Setup
  def setup: Unit = {
    items = DietDataGen.getWorstCaseData

    toSearch = Random.shuffle(items)

    ranges = DietDataGen.getBestCaseData
  }

  @Benchmark
  def dogsDietAdd: Unit = {
    var diet = dogs.Diet.empty[Int]

    items.foreach{ i=>
      diet = diet + i
    }
  }

  @Benchmark
  def scalazDievAdd: Unit = {
    var diev = Diev.empty[Int]

    items.foreach{i =>
      diev = diev + i
    }
  }

  @Benchmark
  def dogsDietAddRange: Unit = {
    var diet = dogs.Diet.empty[Int]

    ranges.foreach {r =>
      diet = diet +  Range(r.start, r.end)
    }
  }

  @Benchmark
  def scalazDievAddRange: Unit = {
    var diev = scalaz.Diev.empty[Int]

    ranges.foreach {r =>
      diev = diev + (r.start, r.end)
    }
  }
}




