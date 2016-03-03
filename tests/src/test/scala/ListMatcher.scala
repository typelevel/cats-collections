/**
  * Created by anicolaspp on 3/2/16.
  */
package dogs
package tests

import dogs.Order.Ordering
import dogs.std.intOrder
import org.scalatest.matchers.{MatchResult, Matcher}
import dogs.Predef._


private [tests] class ListCmp[A](implicit eq: Order[A]) extends Order[List[A]]{

  /**
    * Compares two list item by item
    */
  def cmpLst[B](a: List[B], b: List[B])(implicit eq: Order[B]): scala.Boolean = (a, b) match {
    case (El(), El()) =>  true
    case (El(), _)    =>  false
    case (_, El())    =>  false

    case (Nel(ha, ta), Nel(hb, tb)) => if (eq.eq(ha, hb)) cmpLst(ta, tb) else false
  }

  /**
    * Compare `l` and `r` values returning:
    * Order.LT when `l` is less than `r`
    * Order.EQ when `l` is equal to than `r`
    * Order.GT when `l` is greater than `r`
    */
  override def apply(l: List[A], r: List[A]): Ordering = if (cmpLst(l, r)) dogs.Order.EQ else Order.LT
}

object ListCmp {
  def apply[A](implicit eq: Order[A]): Order[List[A]] = new ListCmp[A]
}


trait ListMatcher {

  private [tests] class ListMatchTo[A](aList: List[A])(implicit eq: Order[A]) extends Matcher[List[A]] {
    override def apply(left: List[A]): MatchResult = {
      val result = ListCmp[A].compare(left, aList) == Order.EQ

      MatchResult(result, "Lists don't match", "")
    }
  }

  def matchTo[A](aList: List[A])(implicit eq: Order[A]) = new ListMatchTo[A](aList)

  implicit val listCmp = ListCmp[Int]
}

