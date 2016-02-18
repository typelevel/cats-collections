package dogs
package bench

import dogs.Order.Ordering
import dogs.Predef._
import dogs.std._
import org.openjdk.jmh.annotations.{Benchmark, Scope, Setup, State}

import scala.util.Random
import scalaz.Diev

/**
  * Created by anicolaspp on 2/18/16.
  */
@State(Scope.Benchmark)
class DietBenchSearch {

  implicit object EnumInt extends Enum[Int] {
    override def succ(x: Int): Int = x + 1
    override def pred(x: Int): Int = x - 1
    override def apply(l: Int, r: Int): Ordering = intOrder(l,r)
  }

  implicit object scalazEnumInt extends scalaz.Enum[Int] {
    override def succ(a: Int): Int = a + 1

    override def pred(a: Int): Int = a - 1

    override def order(x: Int, y: Int): scalaz.Ordering = {
      if (x == y) scalaz.Ordering.EQ
      else if (x < y) scalaz.Ordering.LT
      else scalaz.Ordering.GT
    }
  }

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
