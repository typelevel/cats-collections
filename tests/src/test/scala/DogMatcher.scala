/**
  * Created by anicolaspp on 3/2/16.
  */
package dogs
package tests

import cats.instances.list._
import org.scalatest.matchers.{MatchResult, Matcher}

trait DogMatcher {

  private [tests] class ListMatcher[A](aList: List[A])(implicit eq: Order[A]) extends Matcher[List[A]] {
    override def apply(left: List[A]): MatchResult = {
      val result = Order[List[A]].eqv(left, aList)

      MatchResult(result, s"Lists don't match. Expected: $left Received: $aList" , "")
    }
  }

  object ListMatcher {

    def apply[A](sList: scala.List[A])(implicit eq: Order[A]) = new ListMatcher(sList)

    def sorted[A](aList: List[A])(implicit eq: Order[A]): scala.List[A] =
      aList.sortWith((a, b) => eq.lteqv(a, b))
  }

  private [tests] class DietMatcher[A](aDiet: Diet[A])(implicit eq: Enum[A], order: Order[A]) extends Matcher[Diet[A]] {
    override def apply(left: Diet[A]): MatchResult = {
      val leftList = left.toStreaming.toList
      val rightList = aDiet.toStreaming.toList

      val s = matchTo(rightList)
      val result = s.apply(leftList)

      if (result.matches) {
        result
      }
      else {
        MatchResult(false, "The DIETs don't match. " + result.failureMessage, "")
      }
    }
  }

  def matchTo[A](aList: List[A])(implicit eq: Order[A]) = new ListMatcher[A](aList)

  def matchToSorted[A](aList: List[A])(implicit eq: Order[A]) = ListMatcher[A](ListMatcher.sorted(aList))

  def matchTo[A](aDiet: Diet[A])(implicit eq: Enum[A], order: Order[A]) = new DietMatcher[A](aDiet)
}

