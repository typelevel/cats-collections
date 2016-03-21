package dogs
package bench

import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, Setup, State}
import scala.collection.immutable.{List => SList, Nil => SNil}
import scalaz.IList

trait BigNumberLists {
  @Param(Array("10", "100", "1000", "10000"))
  var n: Int = _

  var dogs: List[Int] = _
  var scala: SList[Int] = _
  var scalazlst: IList[Int] = _

  @Setup
  def setup: Unit = {
    dogs = List.fromIterable(1 to n)
    scala = (1 to n).to[SList]
    scalazlst = IList((1 to n):_*)
  }
}


@State(Scope.Thread)
class ListFlatMapathon extends BigNumberLists {

  @Benchmark def dogsListFlatMapathon(): Unit = {
    val f: Int => List[Int] = _ => new Nel(1, List.empty)
    dogs flatMap f
  }

  @Benchmark def scalaFlatMapathon(): Unit = {
    val f: Int => SList[Int] = _ => 1 :: SNil
    scala flatMap f
  }

  @Benchmark def scalazFlatMapathon(): Unit = {
    val f: Int => IList[Int] = _ => 1 :: IList()
    scalazlst flatMap f
  }
}


@State(Scope.Thread)
class ListFilternium extends BigNumberLists {

  @Benchmark def dogsFilternium(): Unit = {
    dogs.filter(_ % 2 == 0)
  }

  @Benchmark def scalaFilternium(): Unit = {
    scala.filter(_ % 2 == 0)
  }

  @Benchmark def scalazFilternium(): Unit = {
    scalazlst.filter(_ % 2 == 0)
  }
}

@State(Scope.Thread)
class ListFoldlefteron extends BigNumberLists {

  @Benchmark def dogsFoldlefteron(): Unit = {
    dogs.foldLeft(0)(_ + _)
  }

  @Benchmark def scalaFoldlefteron(): Unit = {
    scala.foldLeft(0)(_ + _)
  }

  @Benchmark def scalazFoldlefteron(): Unit = {
    scalazlst.foldLeft(0)(_ + _)
  }
}

@State(Scope.Thread)
class ListMapperfy extends BigNumberLists {

  @Benchmark def dogsMapperfy(): Unit = {
    dogs.map(_ + 1)
  }

  @Benchmark def scalaMapperfy(): Unit = {
    scala.map(_ + 1)
  }

  @Benchmark def scalazMapperfy(): Unit = {
    scalazlst.map(_ + 1)
  }
}
