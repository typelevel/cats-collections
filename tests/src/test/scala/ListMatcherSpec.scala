/**
  * Created by anicolaspp on 3/2/16.
  */

package dogs
package tests

import dogs.Predef._
import dogs.std.intOrder
import org.scalatest.{FlatSpec, Matchers}


class ListMatcherSpec extends FlatSpec with Matchers with ListMatcher {

  "ListMatchWithScalaList" should "match lists" in {

    val a = List(1, 2, 3, 4, 5, 6)
    val matcher = matchTo(a)

    val result = matcher.apply(List(1, 2, 3, 4, 5, 6))

    result.matches should be (true)
  }

  it should "not match with a different list" in {
    val a = List(1, 3, 4, 5)
    val matcher = matchTo(a)

    val result = matcher.apply(List(1, 2, 3, 4, 5))

    result.matches should be (false)
    result.failureMessage should be ("Lists don't match")
  }

  it should "match nested list" in {
    val a: List[List[Int]] = List(List(1), List(2))

    val matcher = matchTo(a)

    val result = matcher.apply(List(List(1), List(2)))

    result.matches should be (true)
  }
}
