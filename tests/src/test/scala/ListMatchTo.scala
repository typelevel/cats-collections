/**
  * Created by anicolaspp on 3/2/16.
  */

package dogs
package tests

import dogs.List
import org.scalatest._
import matchers._

trait ListMatcher {

  class ListMatchWithScalaList[A](aList: List[A]) extends Matcher[List[A]] {

    override def apply(left: List[A]): MatchResult = {
      val result = left.toScalaList == aList.toScalaList

      MatchResult(result, "Lists don't match", "")
    }
  }

  def matchTo[A](aList: List[A]) = new ListMatchWithScalaList[A](aList)
}