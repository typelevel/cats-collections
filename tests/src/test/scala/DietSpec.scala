package dogs
package tests

import dogs.Diet._
import dogs.Predef._
import dogs.std.intOrder
import org.scalacheck.Prop.forAll
import org.scalacheck._
import org.scalatest.{FlatSpec, Matchers}

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
    def remove(d: Diet[Int], i: (Int,Int)): Diet[Int] = d - Range(i._1, i._2)
      //(i._1 to i._2).foldLeft(d)(_ remove _)

    val d = Diet.empty[Int].addRange(Range(r.a1._1, r.a1._2))
      .addRange(Range(r.a2._1, r.a2._2))
      .addRange(Range(r.a3._1, r.a3._2))
      .addRange(Range(r.a4._1, r.a4._2))
      .addRange(Range(r.a5._1, r.a5._2))

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
    val d = Diet.empty[Int] ++ fromRanges(r1) ++ fromRanges(r2)

    (0 to 1000).toList.foreach {i =>
      if(d.contains(i) != (r1.contains(i) || r2.contains(i)))
        println(s"for $i, got ${d.contains(i)} expected ${r1.contains(i)} || ${r2.contains(i)}")
    }

    (0 to 1000).toList.forall(i => d.contains(i) == (r1.contains(i) || r2.contains(i) ))
  }
}

class DietTest extends FlatSpec with Matchers {

  import Diet._
  import dogs.Predef._

  "diet" should "return node with value range when inserting into empty" in {

    val diet = Diet.empty[Int]

    val result = diet.add(5)

    result.min should be (Some(5))
    result.max should be (Some(5))
  }

  it should "have min and max" in {
    val diet = Diet.empty[Int].add(5).add(3).add(7)

    diet.min should be(Some(3))
    diet.max should be(Some(7))
  }

  it should "create a new node when add not adj item" in {
    val diet = Diet.empty[Int].add(5).add(3).add(7)

    val result = diet.intervals.map(l => l.generate.toScalaList).toScalaList

    result should contain inOrderOnly (scala.List(3), scala.List(5), scala.List(7))
  }

  it should "join nodes when item adj to existing seq" in {
    val diet = Diet.empty[Int].add(5).add(6).add(1).add(3).add(2).add(8)

    val result = diet.intervals.map(l => l.generate.toScalaList).toScalaList

    result should contain inOrderOnly (scala.List(1, 2, 3), scala.List(5, 6), scala.List(8))
  }

  it should "be always sorted" in {
    val diet = Diet.empty[Int].add(5).add(6).add(1).add(3).add(2).add(8)

    val sorted = diet.toList().toScalaList

    sorted should contain inOrderOnly (1, 2, 3, 5, 6, 8)
  }

  it should "add disjoint range" in {
    val diet = Diet.empty[Int]

    val result = diet.addRange(Range(0, 100))

    val other = result.intervals.map(l => l.generate.toScalaList).toScalaList


    other should contain ((scala.Range(0, 101)).toList)
  }

  it should "join disjoint range" in {
    val diet = Diet.empty[Int] + 5 + 6 + 7 + 1 + 2

    val other = diet + 3 + 4

    other.toList.toScalaList should contain inOrderOnly (1, 2, 3, 4, 5, 6, 7)
  }

  it should "contain items from range" in {
    val diet = Diet.empty[Int].addRange(Range(5, 10)).addRange(Range(1, 3)).addRange(Range(12, 20))

    diet.contains(1) should be (true)
    diet.contains(2) should be (true)
    diet.contains(3) should be (true)

    diet.contains(4) should be (false)

    diet.contains(6) should be (true)

    diet.contains(15) should be (true)
  }

  it should "return empty when removing from empty" in {
    Diet.empty[Int].remove(1) should be (EmptyDiet())
  }

  it should "not be modified when removing non existed item" in {

    val diet = Diet.empty[Int] + 1 +2 + 3 + 5

    diet.remove(4) should be (diet)
  }

  it should "be split when removing from range" in {
    val diet = Diet.empty[Int] + 1 +2 + 3 + 5

    val other = diet.remove(2).intervals.map(x => x.generate.toScalaList).toScalaList

    other should contain  inOrder (scala.List(1), scala.List(3), scala.List(5))
  }

  it should "map" in {
    val diet = Diet.empty[Int] + 1 +2 + 8 + 5

    val other = diet.map(x => x + 2).intervals.map(x => x.generate.toScalaList).toScalaList

    other should contain inOrderOnly(scala.List(3,4), scala.List(7), scala.List(10))
  }

  it should "foldLeft" in {
    val diet = Diet.empty[Int] + 1 +2 + 8 + 5

    diet.foldLeft(10)(_ + _) should be (26)
    diet.foldRight(10)(_ + _) should be (26)
  }

  it should "contain range" in {
    val x = Diet.empty[Int] + Range(20, 30)

    x.containsRange(Range(20, 30)) should be (true)
    x.containsRange(Range(25, 26)) should be (true)
    x.containsRange(Range(1,10)) should be (false)

    val s = x + Range(10, 15)

    s.containsRange(Range(9, 15)) should be (false)
    s.containsRange(Range(10, 15)) should be (true)
    s.containsRange(Range(9, 16)) should be (false)
  }
}

class DietTestJoin extends FlatSpec with Matchers {

  "diet" should "return the same diet when join to empty range" in {
    val diet = Diet.empty[Int] + 20 + 30

    val range = Range.empty[Int]

    diet.addRange(range) should be (diet)
  }

  it should "return a diet with range when added to empty diet" in {
    val diet = Diet.empty[Int]

    val range = Range(20, 30)

    val other = diet.addRange(range)

    other.min should be (Some(20))
    other.max should be (Some(30))
  }

  it should "increase range to the left" in {
    val diet = Diet.empty[Int] + 20 + 21
    val range = Range(15, 19)

    val other = diet.addRange(range)

    other.intervals.toScalaList(0).generate.toScalaList should contain inOrderOnly (15, 16,17,18,19,20,21)
  }

  it should "create disjoint range to the left" in {
    val diet = Diet.empty[Int] + 20 + 21
    val range = Range(15, 18)

    val sets = diet.addRange(range).intervals.map(r=>r.generate.toScalaList).toScalaList

    sets(0) should contain inOrderOnly(15,16,17,18)
    sets(1) should contain inOrderOnly(20, 21)
  }

  it should "increase range to the right" in {
    val diet = Diet.empty[Int] + 20 + 22
    val range = Range(21, 30)

    val other = diet.addRange(range).intervals.map(r => r.generate.toScalaList).toScalaList

    other should contain (scala.Range(20,31).toList)
  }

  it should "join to an empty diet" in {
    val diet = Diet.empty[Int] + Range(20, 30)

    val other = diet ++ Diet.empty[Int]

    other should be (diet)
  }

  it should "join to another diet" in {
    val diet = Diet.empty[Int] + Range(20, 30)

    val other = diet ++ (Diet.empty[Int] + Range(25, 35) + Range(5, 10) + Range(15, 22))

    val sets = other.intervals.map(r => r.generate.toScalaList).toScalaList

    sets should contain inOrderOnly(
      scala.Range(5,11).toList,
      scala.Range(15, 36).toList
    )

    val otherSets = diet | other

    otherSets.intervals.map(r => r.generate.toScalaList).toScalaList should contain inOrderOnly(
      scala.Range(5,11).toList,
      scala.Range(15, 36).toList
      )
  }

  it should "interset with another diet" in {
    val diet = Diet.empty[Int] + Range(20, 30)

    (diet & Diet.empty[Int]).intervals.toScalaList.length should be (0)

    (diet & diet) should be(diet)

    (diet & (Diet.empty[Int] + Range(15, 25) + Range(28, 32))).toList.toScalaList should
      contain inOrderOnly (20, 21, 22, 23, 24, 25, 28, 29, 30)

    (diet & (Diet.empty[Int] + Range(10, 15))).toList.toScalaList.length should be(0)
  }
}

class DietTestRemove extends FlatSpec with Matchers {
  import Diet._

  "diet" should "remove empty range" in {
    val diet = Diet.empty[Int] + Range(20, 30)

    (diet - Range.empty[Int]) should be(diet)
  }

  it should "return empty when removing from empty" in {

    (Diet.empty[Int] - Range(10, 100)) should be (EmptyDiet())
  }

  it should "remove inner range" in {
    val diet = ((Diet.empty[Int] + Range(20, 30)) - Range(22, 27))

    diet.toList().toScalaList should contain inOrderOnly(20, 21, 28, 29, 30)
  }

  it should "remove side ranges" in {
    val diet = ((Diet.empty[Int]
      + Range(20, 21)
      + Range(9,10)
      + Range(12, 18)
      + Range(23, 30)
      + Range(40, 50)
      + Range(35, 48)
      + Range(55, 60))
      - Range(15, 18)
      - Range(25, 60))

    diet.toList().toScalaList should contain inOrderOnly (9, 10, 12, 13, 14, 20, 21, 23, 24)
  }
}


