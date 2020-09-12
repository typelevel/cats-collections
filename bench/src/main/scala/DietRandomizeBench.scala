/**
  * Created by anicolaspp on 2/18/16.
  */

package cats.collections
package bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scala.util.Random
import scalaz.Diev
import cats._



/**
  * In reality, no one uses the best and worst scenario, so this is a complete randomized benchmark
  */
@State(Scope.Benchmark)
class DietRandomizeBench extends BigNumberLists{

//  import dogs.Predef._

  implicit val scalazEnumInt = scalaz.std.anyVal.intInstance

  @Benchmark
  def dogsDietAddRandom: Unit = {
    Random.shuffle(scala).foldLeft(Diet.empty[Int])((d, r) => d + r)
  }

  @Benchmark
  def scalazDievAddRandom: Unit = {
    Random.shuffle(scalazlst.toList).foldLeft(Diev.empty[Int])((d, r) => d + r)
  }

  @Benchmark
  def dogsDietAddRangeRandom: Unit = {
    Random.shuffle(scala).foldLeft(Diet.empty[Int])((d, r) => d + Range(r, r + 10))
  }

  @Benchmark
  def scalazDievAddRangeRandom: Unit = {
    var diev = Diev.empty[Int]

    Random.shuffle(scalazlst.toList).foldLeft(Diev.empty[Int])((d, r) => d + (r, r + 10))
  }
}
