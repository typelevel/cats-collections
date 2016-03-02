/**
  * Created by anicolaspp on 3/2/16.
  */

package dogs
package tests

import org.scalatest.{Matchers, FlatSpec}

class ListMatchToSpec extends FlatSpec with Matchers with ListMatcher {


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
}
