/**
  * Created by anicolaspp on 3/2/16.
  */

package dogs
package tests

import cats.implicits._

//forcing rebuild

class ListMatcherSpec extends DogsSuite {

  test("match lists"){

    val a = List(1, 2, 3, 4, 5, 6)
    val matcher = matchTo(a)

    val result = matcher.apply(List(1, 2, 3, 4, 5, 6))

    result.matches should be (true)
  }

  test("not match with a different list"){
    val a = List(1, 3, 4, 5)
    val matcher = matchTo(a)

    val other = List(1, 2, 3, 4, 5)
    val result = matcher.apply(other)

    result.matches should be (false)
    result.failureMessage should be (s"Lists don't match. Expected: $other Received: $a")
  }

  test("match nested list"){
    val a: List[List[Int]] = List(List(1), List(2))

    val matcher = matchTo(a)

    val result = matcher.apply(List(List(1), List(2)))

    result.matches should be (true)
  }
}

class DietMatcherSpec extends DogsSuite {

  test("match to another empty diet"){
    val diet = Diet.empty[Int]
    val matcher = matchTo(diet)

    val result = matcher.apply(Diet.empty[Int])

    result.matches should be (true)
  }

  test("match equal diets"){
    val a = Diet.empty[Int] + 2 + 5 + 7
    val matcher = matchTo(a)

    val result = matcher.apply(Diet.empty[Int] + 2 + 5 + 7)

    result.matches should be (true)
  }

  test("not match different diets"){
    val a = Diet.empty[Int] + 2 + 5 + 7
    val matcher = matchTo(a)

    val result = matcher.apply(Diet.empty[Int] + 1 + 2 + 5 + 7)

    result.matches should be (false)
  }
}
