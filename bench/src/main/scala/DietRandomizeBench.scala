/**
  * Created by anicolaspp on 2/18/16.
  */

package dogs
package bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scala.util.Random
import scalaz.Diev


/**
  * In reality, no one uses the best and worst scenario, so this is a complete randomized benchmark
  */
@State(Scope.Benchmark)
class DietRandomizeBench {

  import dogs.Predef._

  implicit val scalazEnumInt = scalaz.std.anyVal.intInstance

  @Benchmark
  def dogsDietAddRandom: Unit = {
    var diet = Diet.empty[Int]

    (1 to 10000).foreach {_ =>
      diet = diet + Random.nextInt()
    }
  }

  @Benchmark
  def scalazDievAddRandom: Unit = {
    var diev = Diev.empty[Int]

    (1 to 10000).foreach {_ =>
      diev = diev + Random.nextInt()
    }
  }

  @Benchmark
  def dogsDietAddRangeRandom: Unit = {
    var diet = Diet.empty[Int]

    (1 to 10000).foreach {_ =>
      val i = Random.nextInt()
      diet = diet + dogs.Range(i, i + 10)
    }
  }

  @Benchmark
  def scalazDievAddRangeRandom: Unit = {
    var diev = Diev.empty[Int]

    (1 to 10000).foreach {_ =>
      val i = Random.nextInt()

      diev = diev + (i, i + 10)
    }
  }
}
