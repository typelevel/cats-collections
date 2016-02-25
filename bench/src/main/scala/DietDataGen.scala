/**
  * Created by anicolaspp on 2/18/16.
  */

package dogs
package bench

import dogs.Predef._

object DietDataGen {

  def getWorstCaseData: scala.IndexedSeq[Int] = scala.Range(1, 1000, 2).to[IndexedSeq]

  def getBestCaseData: scala.IndexedSeq[scala.Range] = {
    for (x <- scala.Range(1, 1000)
         if (x % 10 == 0)
    ) yield scala.Range(x, x + 10)
  }
}
