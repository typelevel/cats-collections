/**
  * Created by anicolaspp on 2/18/16.
  */

package dogs
package bench

import dogs.Predef._

object DietDataGen {

  def getWorstCaseData: scala.IndexedSeq[Int] = {
    for (x <- scala.Range(1, 1000)
      if (x % 2 == 0)
    ) yield x
  }

  def getBestCaseData: scala.IndexedSeq[scala.Range] = {
    var x = 0

    for (x <- scala.Range(1, 1000)
         if (x % 10 == 0)
    ) yield scala.Range(x, x + 10)
  }
}
