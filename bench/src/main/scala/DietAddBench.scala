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

  implicit val scalazEnumInt = scalaz.std.anyVal.intInstance

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




