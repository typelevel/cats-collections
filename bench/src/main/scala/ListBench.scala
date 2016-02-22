package dogs
package bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scala.collection.immutable.{List => SList, Nil => SNil}
import scalaz.{IList,ICons,INil}

trait BigNumberLists {
  val dogs = List.fromIterable(1 to 10000)
  val scala = (1 to 10000).to[SList]
  val scalaz = IList((1 to 10000):_*)
}


@State(Scope.Thread)
class ListFlatMapathon {

  @Benchmark def dogsListFlatMapathon(): Unit = {
    val f: Int => List[Int] = _ => new Nel(1, List.empty)

    var i = 0
    var l: List[Int]  = List(1)
    while(i < 10000) {
      i = i + 1
      l = l flatMap f
    }
  }


  @Benchmark def scalaFlatMapathon(): Unit = {
    val f: Int => SList[Int] = _ => 1 :: SNil

    var i = 0
    var l = SList(1)
    while(i < 10000) {
      i = i + 1
      l = l flatMap f
    }
  }

  @Benchmark def scalazFlatMapathon(): Unit = {
    val f: Int => IList[Int] = _ => 1 :: IList()

    var i = 0
    var l = IList(1)
    while(i < 10000) {
      i = i + 1
      l = l flatMap f
    }
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
    scalaz.filter(_ % 2 == 0)
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
    scalaz.foldLeft(0)(_ + _)
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
    scalaz.map(_ + 1)
  }
}
