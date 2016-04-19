package dogs
package tests

import dogs.Predef._

class RangeTest  extends DogsSpec {

  import Range._

  import cats.syntax.all._

  import cats.std.int._


  "a range" should "contain items within [start, end]" in {
    val range = Range(1, 100)

    scala.Range(1,100).foreach(i => range.contains(i) should be (true))
  }

  it should "not contain items outside [start, end]" in {
    val range = Range(1, 100)

    range.contains(0) should be (false)

    range.contains(101) should be (false)
  }

  it should "contain sub range" in {
    val range = Range(1, 10)

    range.contains(Range(2,9)) should be (true)
  }

  it should "apply function to each element" in {
    var s = 0

    val range = Range(1, 100)

    range.foreach(i => s += i)

    scala.Range(1,101).sum should be (s)
  }

  it should "map" in {
    val range = Range(1, 100)
    val result = range.map(i => i.toString)

    scala.Range(1,101).map(i => i.toString).toList should be (result.toScalaList)
  }

  it should "fold" in {
    val range = Range(1, 100)

    range.foldLeft[Int](0, (a,b) => a + b) should be (scala.Range(1,101).sum)
  }

  it should "be able to diff (-)" in {
    val range = Range(1, 10)

    val (l, r) = range - Range(2,9)

    l.generate.toScalaList should be(scala.List(1))
    r.generate.toScalaList should be(scala.List(10))

    val (l1, r1) = range - range

    l1 should be (Range.empty())
    r1 should be (Range.empty())

    val (l2, r2) = range - Range(-1, 5)

    l2 should be (Range.empty())
    r2.generate.toScalaList should contain inOrderOnly (6, 7, 8, 9, 10)

    val (l3, r3) = range - Range(3, 12)

    l3.generate.toScalaList should contain inOrderOnly(1, 2)
    r3 should be (Range.empty())
  }

  it should "generate inverted range" in {
    val range = Range(5, 1)

    range.generate.toScalaList should contain inOrderOnly(5, 4, 3, 2, 1)
  }

  it should "map inverted range" in {
    val range = Range(5, 1)

    val result = range.map(i => i.toString)

    result.toScalaList should contain inOrderOnly ("5", "4", "3", "2", "1")
  }

  it should "the reverse be equals to the reverted range" in {
    val range = Range (20, 50)
    val other = Range (50, 20)

    range.reverse.toList.toScalaList should be (other.toList.toScalaList)
  }

  it should "be convertible to string in math form when empty" in {
    val range = Range.empty[Int]

    range.show should be ("[]")
  }

  it should "be convertible to string in math form" in {
    val range = Range(10, 20)

    range.show should be ("[10, 20]")
  }
}
