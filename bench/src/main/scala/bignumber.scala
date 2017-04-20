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
    dogs = (1 to n).toList
    scala = (1 to n).to[SList]
    scalazlst = IList((1 to n):_*)
  }
}


