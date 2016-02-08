/**
  * Created by anicolaspp on 2/8/16.
  */

package dogs

import dogs.Order.Ordering
import dogs.Predef._
import dogs.std._
import org.scalacheck.Properties
import org.scalatest.{FlatSpec, Matchers}

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary,Gen,Properties}

class ARangeTest extends FlatSpec with Matchers {
  implicit object EnumInt extends Enum[Int] {
    override def succ(x: Int): Int = x + 1
    override def pred(x: Int): Int = x - 1
    override def apply(l: Int, r: Int): Ordering = intOrder(l,r)
  }

  "a range" should "contain items within [start, end]" in {
    val range = ARange(1, 100)

    1.to(100).foreach(i => range.contains(i) should be (true))
  }

  it should "not contain items outside [start, end]" in {
    val range = ARange(1, 100)

    range.contains(0) should be (false)

    range.contains(101) should be (false)
  }

  it should "contain sub range" in {
    val range = ARange(1, 10)

    range.contains(ARange(2,9)) should be (true)
  }

  it should "apply function to each element" in {
    var s = 0

    val range = ARange(1, 100)

    range.foreach(i => s += i)

    1.to(100).sum should be (s)
  }

  it should "map" in {
    val range = ARange(1, 100)

    val result = range.map(i => i.toString)

    1.to(100).map(i => i.toString).toList should be (result.toScalaList)
  }

  it should "fold" in {
    val range = ARange(1, 100)

    range.foldLeft[Int](0, (a,b) => a + b) should be (1.to(100).sum)
  }

  it should "be able to diff (-)" in {
    val range = ARange(1, 10)

    val (l, r) = range - ARange(2,9)

    l.generate().toScalaList should be(scala.List(1))
    r.generate().toScalaList should be(scala.List(10))

    val (l1, r1) = range - range

    l1 should be (ARange.empty())
    r1 should be (ARange.empty())

    val (l2, r2) = range - ARange(-1, 5)

    l2 should be (ARange.empty())
    r2.generate().toScalaList should contain inOrderOnly (6, 7, 8, 9, 10)

    val (l3, r3) = range - ARange(3, 12)

    l3.generate().toScalaList should contain inOrderOnly(1, 2)
    r3 should be (ARange.empty())
  }
}
