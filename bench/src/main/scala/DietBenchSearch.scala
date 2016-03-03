/**
 * Created by anicolaspp on 2/18/16.
 */

package dogs
package bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, Setup, State}

import scala.util.Random
import scalaz.Diev
import algebra.std.int._

@State(Scope.Benchmark)
class DietBenchSearch {

  import dogs.Predef._

  implicit val scalazEnumInt = scalaz.std.anyVal.intInstance

  var diet = Diet.empty[Int]
  var diev = Diev.empty[Int]

  @Setup
  def setup: Unit = {
    var i = 0
    while (i < 1000) {
      val s = Random.nextInt()
      val e = s + Random.nextInt()

      diet = diet + Range(s, e)
      diev = diev + (s, e)

      i = i + 1
    }
  }

  @Benchmark
  def dogsDietSearch: Unit = {
    scala.Range(0, 1000).foreach(i => diet.contains(i))
  }

  @Benchmark
  def scalazDievSearch: Unit = {
    scala.Range(0, 1000).foreach(i => diev.contains(i))
  }
}
