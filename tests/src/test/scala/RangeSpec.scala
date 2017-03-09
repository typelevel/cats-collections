package dogs
package tests

import dogs.Predef._

class RangeTest extends DogsSuite {

  import Range._

  import cats.implicits._

  test("contain items within [start, end]"){
    val range = Range(1, 100)

    scala.Range(1,100).foreach(i => range.contains(i) should be (true))
  }

  test("not contain items outside [start, end]"){
    val range = Range(1, 100)

    range.contains(0) should be (false)

    range.contains(101) should be (false)
  }

  test("contain sub range"){
    val range = Range(1, 10)

    range.contains(Range(2,9)) should be (true)
  }

  test("apply function to each element"){
    var s = 0

    val range = Range(1, 100)

    range.foreach(i => s += i)

    scala.Range(1,101).sum should be (s)
  }

  test("map"){
    val range = Range(1, 10)

    val result = range.map(_ * 2).toList

    scala.Range(2, 21).toList should be (result.toScalaList)
  }

  test("fold"){
    val range = Range(1, 100)

    range.foldLeft[Int](0, (a,b) => a + b) should be (scala.Range(1,101).sum)
  }

  test("be able to diff (-)"){
    val range = Range(1, 10)

    val Some((l, Some(r))) = range - Range(2,9)

    l.toStreaming.toList should matchTo (List(1))
    r.toStreaming.toList should matchTo (List(10))

    val x1 = range - range

    x1.isDefined should be (false)

    val Some((x2, None)) = range - Range(-1, 5)

    x2.toStreaming.toScalaList should contain inOrderOnly (6, 7, 8, 9, 10)

    val Some((x3, None)) = range - Range(3, 12)

    x3.toStreaming.toScalaList should contain inOrderOnly(1, 2)
  }

  test("generate inverted range"){
    val range = Range(5, 1)

    range.toStreaming.toScalaList should contain inOrderOnly(5, 4, 3, 2, 1)
  }

  test("map inverted range"){
    val range = Range(5, 1)

    val result = range.map(_ * 2).toList

    result.toScalaList should contain inOrderOnly (10, 9, 8, 7, 6, 5, 4, 3, 2)
  }

  test("the reverse be equals to the reverted range"){
    val range = Range (20, 50)
    val other = Range (50, 20)

    range.reverse.toList.toScalaList should be (other.toList.toScalaList)
  }

  test("be convertible to string in math form"){
    val range = Range(10, 20)

    range.show should be ("[10, 20]")
  }
}
