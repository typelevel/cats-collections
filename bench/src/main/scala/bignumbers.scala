package cats.collections
package bench

import scalaz.IList
import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, Setup, State}


trait BigNumberLists {
  @Param(Array("10", "100", "1000", "10000"))
  var n: Int = _

  var scala: List[Int] = _
  var scalazlst: IList[Int] = _

  @Setup
  def setup: Unit = {
    scala = (1 to n).toList
    scalazlst = IList((1 to n):_*)
  }
}
