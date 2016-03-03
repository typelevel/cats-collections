/**
  * Created by anicolaspp on 3/2/16.
  */

package dogs
package tests

import dogs.Predef._
import dogs.std.intOrder
import org.scalatest.{FlatSpec, Matchers}


class ListMatcherSpec extends FlatSpec with Matchers with DogMatcher {

  "ListMatchWithScalaList" should "match lists" in {

    val a = List(1, 2, 3, 4, 5, 6)
    val matcher = matchTo(a)

    val result = matcher.apply(List(1, 2, 3, 4, 5, 6))

    result.matches should be (true)
  }

  it should "not match with a different list" in {
    val a = List(1, 3, 4, 5)
    val matcher = matchTo(a)

    val other = List(1, 2, 3, 4, 5)
    val result = matcher.apply(other)

    result.matches should be (false)
    result.failureMessage should be (s"Lists don't match. Expected: $other Received: $a")
  }

  it should "match nested list" in {
    val a: List[List[Int]] = List(List(1), List(2))

    val matcher = matchTo(a)

    val result = matcher.apply(List(List(1), List(2)))

    result.matches should be (true)
  }
}

class DietMatcherSpec extends FlatSpec with Matchers with DogMatcher {

  "Diet" should "match to another empty diet" in {
    val diet = Diet.empty[Int]
    val matcher = matchTo(diet)

    val result = matcher.apply(Diet.empty[Int])

    result.matches should be (true)
  }

  it should "match equal diets" in {
    val a = Diet.empty[Int] + 2 + 5 + 7
    val matcher = matchTo(a)

    val result = matcher.apply(Diet.empty[Int] + 2 + 5 + 7)

    result.matches should be (true)
  }

  it should "not match different diets" in {
    val a = Diet.empty[Int] + 2 + 5 + 7
    val matcher = matchTo(a)

    val result = matcher.apply(Diet.empty[Int] + 1 + 2 + 5 + 7)

    result.matches should be (false)
  }
}
