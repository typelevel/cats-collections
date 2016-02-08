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

  it should "apply function to each elemet" in {
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
}
