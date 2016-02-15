package dogs
package bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class ListPrependBench {

  @Benchmark
  def dogsSum(): Int = {
    var i = 0
    var l: dogs.List[Int] = dogs.El()
    while(i < 100000) {
      l = i :: l
      i = i + 1
    }

    def sum(l: List[Int], s: Int): Int = l match {
      case El() => s
      case h Nel t => sum(t, s + h)
    }

    sum(l, 0)
  }

  @Benchmark
  def scalaSum(): Int = {
    var i = 0
    var l: scala.List[Int] = scala.Nil
    while(i < 100000) {
      l = i :: l
      i = i + 1
    }

    def sum(l: scala.List[Int], s: Int): Int = l match {
      case Nil => s
      case h :: t => sum(t, s + h)
    }

    sum(l, 0)
  }

}
