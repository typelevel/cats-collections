package cats.collections
package tests

import cats.tests.CatsSuite

class RangeTest extends CatsSuite {

  import Range._

  test("contain items within [start, end]") {
    val range = Range(1, 100)

    scala.Range(1, 100).foreach(i => range.contains(i) should be(true))
  }

  test("not contain items outside [start, end]") {
    val range = Range(1, 100)

    range.contains(0) should be(false)

    range.contains(101) should be(false)
  }

  test("contain sub range") {
    val range = Range(1, 10)

    range.contains(Range(2, 9)) should be(true)
  }

  test("apply function to each element") {
    var s = 0

    val range = Range(1, 100)

    range.foreach(i => s += i)

    scala.Range(1, 101).sum should be(s)
  }

  test("map") {
    val range = Range(1, 10)

    val result = range.map(_ * 2).toList.sorted

    scala.Range(2, 21).toList.sorted should be(result)
  }

  test("foldLeft") {
    val range = Range(1, 100)

    range.foldLeft[Int](0, (a, b) => a + b) should be(scala.Range(1, 101).sum)
    range.foldLeft[Int](0, (_, b) => b) should be(100)
  }

  test("foldLeft in the right order") {
    val range = Range(1, 100)

    range.foldLeft[Int](0, (_, b) => b) should be(100)
  }

  test("foldRight") {
    val range = Range(1, 100)

    range.foldRight[Int](0, (a, b) => a + b) should be(scala.Range(1, 101).sum)
  }

  test("foldRight in the right order") {
    val range = Range(1, 100)

    range.foldRight[Int](0, (a, _) => a) should be(1)
  }

  test("be able to diff (-)") {
    val range = Range(1, 10)

    val Some((l, Some(r))) = range - Range(2, 9)

    l.toList should be(List(1))
    r.toList should be(List(10))

    val x1 = range - range

    x1.isDefined should be(false)

    val Some((x2, None)) = range - Range(-1, 5)

    (x2.toList should contain).inOrderOnly(6, 7, 8, 9, 10)

    val Some((x3, None)) = range - Range(3, 12)

    (x3.toList should contain).inOrderOnly(1, 2)
  }

  test("return an iterator for the range") {
    Range(0, 10).toIterator.toList shouldEqual List.range(0, 11) // [0, 10]
  }

  test("return an iterator for a reversed range") {
    Range(10, 0).toIterator.toList shouldEqual List.range(10, -1, -1) // [10, 0]
  }

  test("return an iterator when the range contains a single value") {
    Range(3, 3).toIterator.toList shouldEqual List(3)
  }

  test("generate inverted range") {
    val range = Range(5, 1)

    (range.toList should contain).inOrderOnly(5, 4, 3, 2, 1)
  }

  test("map inverted range") {
    val range = Range(5, 1)

    val result = range.map(_ * 2).toList

    (result should contain).inOrderOnly(10, 9, 8, 7, 6, 5, 4, 3, 2)
  }

  test("the reverse be equals to the reverted range") {
    val range = Range(20, 50)
    val other = Range(50, 20)

    range.reverse.toList should be(other.toList)
  }

  test("be convertible to string in math form") {
    val range = Range(10, 20)

    range.show should be("[10, 20]")
  }
}
