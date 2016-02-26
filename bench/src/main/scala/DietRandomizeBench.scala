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
class DietRandomizeBench extends BigNumberLists{

//  import dogs.Predef._

  implicit val scalazEnumInt = scalaz.std.anyVal.intInstance

  @Benchmark
  def dogsDietAddRandom: Unit = {
    var diet = Diet.empty[Int]

    Random.shuffle(dogs.toScalaList).foreach{ i => diet = diet + i }
  }

  @Benchmark
  def scalazDievAddRandom: Unit = {
    var diev = Diev.empty[Int]

    Random.shuffle(scalazlst.toList).foreach { i => diev = diev + i}
  }

  @Benchmark
  def dogsDietAddRangeRandom: Unit = {
    var diet = Diet.empty[Int]

    Random.shuffle(dogs.toScalaList).foreach { i => diet = diet + Range(i, i + 10)}
  }

  @Benchmark
  def scalazDievAddRangeRandom: Unit = {
    var diev = Diev.empty[Int]

    Random.shuffle(scalazlst.toList).foreach { i => diev = diev + (i, i + 10)}
  }
}
