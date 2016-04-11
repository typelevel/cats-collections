/**
  * Created by anicolaspp on 4/10/16.
  */

import dogs.{Sorted, El}
import org.openjdk.jmh.annotations.{Benchmark, Setup, Scope, State}
import scala.util.Random
import cats.implicits._
import dogs.Predef._


@State(Scope.Benchmark)
class SortedBench {

  var l = El[Int]

  @Setup
  def setup: Unit = {
    var i = 0
    while (i < 100000) {
      val s = Random.nextInt()

      l = l.::(s)

      i = i + 1
    }
  }

  @Benchmark
  def quicksort: Unit = {
    Sorted.quickSort(l)
  }

  @Benchmark
  def heapsort: Unit = {
    Sorted.heapSort(l)
  }
}
