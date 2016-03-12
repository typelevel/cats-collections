/**
  * Created by anicolaspp on 3/2/16.
  */
package dogs
package tests

import cats.Order
import algebra.std.int._
import org.scalatest.matchers.{MatchResult, Matcher}
import dogs.Predef._

trait DogMatcher {

  private [tests] class ListMatcher[A](aList: List[A])(implicit eq: Order[A]) extends Matcher[List[A]] {
    override def apply(left: List[A]): MatchResult = {
      val result = Order[List[A]].eqv(left, aList)

      MatchResult(result, s"Lists don't match. Expected: $left Received: $aList" , "")
    }
  }

  private [tests] class DietMatcher[A](aDiet: Diet[A])(implicit eq: Enum[A], order: Order[A]) extends Matcher[Diet[A]] {
    override def apply(left: Diet[A]): MatchResult = {
      val leftList = left.intervals.map(r => r.generate)
      val rightList = aDiet.intervals.map(r => r.generate)

      val s = matchTo(rightList)
      val result = s.apply(leftList)

      if (result.matches) {
        result
      }
      else {
        MatchResult(false, "The intervals don't match. " + result.failureMessage, "")
      }
    }
  }

  def matchTo[A](aList: List[A])(implicit eq: Order[A]) = new ListMatcher[A](aList)

  def matchTo[A](aDiet: Diet[A])(implicit eq: Enum[A], order: Order[A]) = new DietMatcher[A](aDiet)
}

