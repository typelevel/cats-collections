package cats.collections

import cats.syntax.show._
import munit.FunSuite

class RangeSuite extends FunSuite {
  import Range._

  test("contain items within [start, end]"){
    val range = Range(1, 100)

    assert(scala.Range(1,100).forall(i => range.contains(i)))
  }

  test("not contain items outside [start, end]"){
    val range = Range(1, 100)

    assertEquals(range.contains(0), false)

    assertEquals(range.contains(101), false)
  }

  test("contain sub range"){
    val range = Range(1, 10)

    assert(range.contains(Range(2,9)))
  }

  test("apply function to each element"){
    var s = 0

    val range = Range(1, 100)

    range.foreach(i => s += i)

    assertEquals(scala.Range(1,101).sum, s)
  }

  test("map"){
    val range = Range(1, 10)

    val result = range.map(_ * 2).toList.sorted

    assertEquals(scala.Range(2, 21).toList.sorted, result)
  }

  test("foldLeft"){
    val range = Range(1, 100)

    assertEquals(range.foldLeft[Int](0, (a,b) => a + b), scala.Range(1,101).sum)
    assertEquals(range.foldLeft[Int](0, (_,b) => b), 100)
  }

  test("foldLeft in the right order"){
    val range = Range(1, 100)

    assertEquals(range.foldLeft[Int](0, (_,b) => b), 100)
  }

  test("foldRight"){
    val range = Range(1, 100)

    assertEquals(range.foldRight[Int](0, (a,b) => a + b), scala.Range(1,101).sum)
  }

  test("foldRight in the right order"){
    val range = Range(1, 100)

    assertEquals(range.foldRight[Int](0, (a,_) => a), 1)
  }

  test("be able to diff (-)"){
    val range = Range(1, 10)

    val Some((l, Some(r))) = range - Range(2,9)

    assertEquals(l.toList, List(1))
    assertEquals(r.toList, List(10))

    val x1 = range - range

    assertEquals(x1.isDefined, false)

    val Some((x2, None)) = range - Range(-1, 5)

    assertEquals(x2.toList, List(6, 7, 8, 9, 10))

    val Some((x3, None)) = range - Range(3, 12)

    assertEquals(x3.toList, List(1, 2))
  }

  test("return an iterator for the range") {
    assertEquals(Range(0, 10).toIterator.toList, List.range(0, 11)) // [0, 10]
  }

  test("return an iterator for a reversed range") {
    assertEquals(Range(10, 0).toIterator.toList, List.range(10, -1, -1)) // [10, 0]
  }

  test("return an iterator when the range contains a single value") {
    assertEquals(Range(3, 3).toIterator.toList, List(3))
  }

  test("generate inverted range"){
    val range = Range(5, 1)

    assertEquals(range.toList, List(5, 4, 3, 2, 1))
  }

  test("map inverted range"){
    val range = Range(5, 1)

    val result = range.map(_ * 2).toList

    assertEquals(result, List(10, 9, 8, 7, 6, 5, 4, 3, 2))
  }

  test("the reverse be equals to the reverted range"){
    val range = Range (20, 50)
    val other = Range (50, 20)

    assertEquals(range.reverse.toList, other.toList)
  }

  test("be convertible to string in math form"){
    val range = Range(10, 20)

    assertEquals(range.show, "[10, 20]")
  }
}
