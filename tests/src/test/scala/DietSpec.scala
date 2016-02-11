package dogs
package tests

import dogs.Diet._
import dogs.Predef._
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalatest.{FlatSpec, Matchers}
import dogs.std.intOrder
import dogs.Order._

object DietSpec extends Properties("Diet") {

  // we'll pick 8 ranges between 0 and 1000,
  // 5 which we'll add, then 3 which we'll remove
  case class Ranges(a1: (Int,Int),
                    a2: (Int,Int),
                    a3: (Int,Int),
                    a4: (Int,Int),
                    a5: (Int,Int),
                    r1: (Int,Int),
                    r2: (Int,Int),
                    r3: (Int,Int)
  ) {
    def in(i: Int)(r: (Int,Int)): Boolean = i >= r._1 && i <= r._2

    // test if we think any given I *should* be returns as in the Diet
    def contains(i: Int) = {
      val f = in(i) _
      (f(a1) | f(a2) | f(a3) | f(a4) | f(a5)) && !(f(r1)| f(r2) | f(r3))
    }
  }

  implicit object EnumInt extends Enum[Int] {
    override def succ(x: Int): Int = x + 1
    override def pred(x: Int): Int = x - 1
    override def apply(l: Int, r: Int): Ordering = intOrder(l,r)

  }

  def orderedTuple(x: Int, y: Int): (Int,Int) =
    intOrder(x,y) match {
      case GT => y -> x
      case _ => x -> y
    }

  implicit val arbNine: Arbitrary[Ranges] = Arbitrary(
    for {
      i1  <- Gen.choose(0,9999)
      i2  <- Gen.choose(0,9999)
      i3  <- Gen.choose(0,9999)
      i4  <- Gen.choose(0,9999)
      i5  <- Gen.choose(0,9999)
      i6  <- Gen.choose(0,9999)
      i7  <- Gen.choose(0,9999)
      i8  <- Gen.choose(0,9999)
      i9  <- Gen.choose(0,9999)
      i10 <- Gen.choose(0,9999)
      i11 <- Gen.choose(0,9999)
      i12 <- Gen.choose(0,9999)
      i13 <- Gen.choose(0,9999)
      i14 <- Gen.choose(0,9999)
      i15 <- Gen.choose(0,9999)
      i16 <- Gen.choose(0,9999)
    } yield Ranges(orderedTuple(i1, i2),
                   orderedTuple(i3, i4),
                   orderedTuple(i5, i6),
                   orderedTuple(i7, i8),
                   orderedTuple(i9, i10),
                   orderedTuple(i11, i12),
                   orderedTuple(i13, i14),
                   orderedTuple(i15, i16))
  )

  def fromRanges(r: Ranges): Diet[Int] = {
    // hmm, can't remove a range, have to do it one by one
    def remove(d: Diet[Int], i: (Int,Int)): Diet[Int] =
      (i._1 to i._2).foldLeft(d)(_ remove _)

    val d = Diet[Int].add(r.a1._1, r.a1._2)
      .add(r.a2._1, r.a2._2)
      .add(r.a3._1, r.a3._2)
      .add(r.a4._1, r.a4._2)
      .add(r.a5._1, r.a5._2)

    remove(remove(remove(d, r.r1), r.r2), r.r3)
  }

  property("diet") = forAll { (r: Ranges) =>
    val d = fromRanges(r)

    (0 to 1000).toList.foreach {i =>
      if(d.contains(i) != r.contains(i)) {
        println(s"for $i, got ${d.contains(i)} expected ${r.contains(i)}")
      }
    }

    (0 to 1000).toList.forall(i => d.contains(i) == r.contains(i))
  }

  property("merge") = forAll{ (r1: Ranges, r2: Ranges) =>
    val d = Diet[Int] ++ fromRanges(r1) ++ fromRanges(r2)

    (0 to 1000).toList.foreach {i =>
      if(d.contains(i) != (r1.contains(i) || r2.contains(i)))
        println(s"for $i, got ${d.contains(i)} expected ${r1.contains(i)} || ${r2.contains(i)}")
    }

    (0 to 1000).toList.forall(i => d.contains(i) == (r1.contains(i) || r2.contains(i) ))
  }
}

class DietTest extends FlatSpec with Matchers {

  import dogs.Predef._

  implicit val d = new BigIntEnum()

  "diet" should "return node with value range when inserting into empty" in {

    val diet = Diet[BigInt]

    val result = diet.add(5)

    result.min should be (Some(5))
    result.max should be (Some(5))
  }

  it should "have min and max" in {
    val diet = Diet[BigInt].add(5).add(3).add(7)

    diet.min should be(Some(3))
    diet.max should be(Some(7))
  }

  it should "create a new node when add not adj item" in {
    val diet = Diet[BigInt].add(5).add(3).add(7)

    val result = diet.disjointSets.map(l => l.generate().toScalaList).toScalaList

    result should contain inOrderOnly (scala.List(3), scala.List(5), scala.List(7))
  }

  it should "join nodes when item adj to existing seq" in {
    val diet = Diet[BigInt].add(5).add(6).add(1).add(3).add(2).add(8)

    val result = diet.disjointSets.map(l => l.generate().toScalaList).toScalaList

    result should contain inOrderOnly (scala.List(1, 2, 3), scala.List(5, 6), scala.List(8))
  }

  it should "be always sorted" in {
    val diet = Diet[BigInt].add(5).add(6).add(1).add(3).add(2).add(8)

    val sorted = diet.toList().toScalaList

    sorted should contain inOrderOnly (1, 2, 3, 5, 6, 8)
  }

  it should "add disjoint range" in {
    val diet = Diet[BigInt]

    val result = diet.add(0, 100)

    val other = result.disjointSets.map(l => l.generate().toScalaList).toScalaList

    other should contain ((scala.Range(0, 101)).toList)
  }

  it should "join disjoint range" in {
    val diet = Diet[BigInt] + 5 + 6 + 7 + 1 + 2

    val other = diet + 3 + 4

    other.toList.toScalaList should contain inOrderOnly (1, 2, 3, 4, 5, 6, 7)
  }

  it should "contain items from range" in {
    val diet = Diet[BigInt].add(5, 10).add(1, 3).add(12, 20)

    diet.contains(1) should be (true)
    diet.contains(2) should be (true)
    diet.contains(3) should be (true)

    diet.contains(4) should be (false)

    diet.contains(6) should be (true)

    diet.contains(15) should be (true)
  }

  it should "return empty when removing from empty" in {
    Diet[BigInt].remove(1) should be (EmptyDiet())
  }

  it should "not be modified when removing non existed item" in {

    val diet = Diet[BigInt] + 1 +2 + 3 + 5

    diet.remove(4) should be (diet)
  }

  it should "be split when removing from range" in {
    val diet = Diet[BigInt] + 1 +2 + 3 + 5

    val other = diet.remove(2).disjointSets.map(x => x.generate().toScalaList).toScalaList

    other should contain  inOrder (scala.List(1), scala.List(3), scala.List(5))
  }

  it should "map" in {
    val diet = Diet[BigInt] + 1 +2 + 8 + 5

    val other = diet.map(x => x + 2).disjointSets.map(x => x.generate().toScalaList).toScalaList

    other should contain inOrderOnly(scala.List(3,4), scala.List(7), scala.List(10))
  }

  it should "foldLeft" in {
    val diet = Diet[BigInt] + 1 +2 + 8 + 5

    diet.foldLeft(BigInt(10))((a: BigInt,b: BigInt) => a + b) should be (26)
    diet.foldRight(BigInt(10))((a: BigInt,b: BigInt) => a + b) should be (26)
  }

  it should "contain range" in {
    val x = Diet[BigInt] + (20, 30)

    x.contains(Range[BigInt](20, 30)) should be (true)
    x.contains(Range[BigInt](25, 26)) should be (true)
    x.contains(Range[BigInt](1,10)) should be (false)

    val s = x + (10, 15)

    s.contains(Range[BigInt](9, 15)) should be (false)
    s.contains(Range[BigInt](10, 15)) should be (true)
    s.contains(Range[BigInt](9, 16)) should be (false)
  }
}

class DietTestJoin extends FlatSpec with Matchers {
  implicit val d = new BigIntEnum()

  "diet" should "return the same diet when join to empty range" in {
    val diet = Diet[BigInt] + 20 + 30

    val range = Range.empty[BigInt]

    diet.add(range) should be (diet)
  }

  it should "return a diet with range when added to empty diet" in {
    val diet = Diet[BigInt]

    val range = Range[BigInt](20, 30)

    val other = diet.add(range)

    other.min should be (Some(20))
    other.max should be (Some(30))
  }

  it should "increase range to the left" in {
    val diet = Diet[BigInt] + 20 + 21
    val range = Range[BigInt](15, 19)

    val other = diet.add(range)

    other.disjointSets.toScalaList(0).generate().toScalaList should contain inOrderOnly (15, 16,17,18,19,20,21)
  }

  it should "create disjoint range to the left" in {
    val diet = Diet[BigInt] + 20 + 21
    val range = Range[BigInt](15, 18)

    val sets = diet.add(range).disjointSets.map(r=>r.generate().toScalaList).toScalaList

    sets(0) should contain inOrderOnly(15,16,17,18)
    sets(1) should contain inOrderOnly(20, 21)
  }

  it should "increase range to the right" in {
    val diet = Diet[BigInt] + 20 + 22
    val range = Range[BigInt](21, 30)

    val other = diet.add(range).disjointSets.map(r => r.generate().toScalaList).toScalaList

    other should contain (20.to(30).toList)
  }

  it should "join to an empty diet" in {
    val diet = Diet[BigInt] + (20, 30)

    val other = diet ++ Diet.empty[BigInt]

    other should be (diet)
  }

  it should "join to another diet" in {
    val diet = Diet[BigInt] + (20, 30)

    val other = diet ++ (Diet[BigInt] + (25, 35) + (5, 10) + (15, 22))

    val sets = other.disjointSets.map(r => r.generate().toScalaList).toScalaList

    sets should contain inOrderOnly(
      scala.Range(5,11).toList,
      scala.Range(15, 36).toList
    )
  }
}

