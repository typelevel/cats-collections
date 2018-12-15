package cats.collections
package tests

import catalysts.Platform
import cats._
import org.scalacheck._
import cats.tests.CatsSuite
import org.scalactic.anyvals.PosInt

class DietSpec extends CatsSuite {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration.copy(
      minSuccessful = if (Platform.isJvm) PosInt(10000) else PosInt(500)
    )

  sealed trait Item {
    def toSet: Set[Int]
    def addToDiet(d: Diet[Int]): Diet[Int]
    def removeFromDiet(d: Diet[Int]): Diet[Int]
  }
  object Item {
    case class Single(n: Int) extends Item {
      def toSet: Set[Int] = Set(n)
      def addToDiet(d: Diet[Int]): Diet[Int] = d.add(n)
      def removeFromDiet(d: Diet[Int]): Diet[Int] = d.remove(n)
    }
    case class Multiple(x: Int, y: Int) extends Item {
      def toRange: Range[Int] =
        if (x <= y) Range(x, y)
        else Range(y, x)
      def toSet: Set[Int] =
        if (x <= y) (x to y).toSet
        else (y to x).toSet
      def addToDiet(d: Diet[Int]): Diet[Int] = d.addRange(toRange)
      def removeFromDiet(d: Diet[Int]): Diet[Int] = d.removeRange(toRange)
    }
  }

  implicit val arbItem: Arbitrary[Item] =
    Arbitrary(
      Gen.oneOf(
        Gen.choose(0, 999).map(Item.Single),
        for (min <- Gen.choose(0, 999); max <- Gen.choose(0, 999)) yield Item.Multiple(min, max)
      )
    )

  case class Ranges(rs: List[(Boolean, Item)]) {
    def isEmpty: Boolean = toSet.isEmpty
    def toSet: Set[Int] = rs.foldLeft(Set.empty[Int]) { (set, r) =>
      val s = r._2.toSet
      if (r._1) set.union(s)
      else set.diff(s)
    }
    def toDiet: Diet[Int] = rs.foldLeft(Diet.empty[Int]) { (diet, r) =>
      if (r._1) r._2.addToDiet(diet)
      else r._2.removeFromDiet(diet)
    }
  }

  implicit val arbRanges: Arbitrary[Ranges] =
    Arbitrary(
      Gen.listOf {
        for {
          inout <- Gen.oneOf(true, false)
          item <- Arbitrary.arbitrary[Item]
        } yield (inout, item)
      }.map(Ranges)
    )

  test("shown empty"){
    val diet = Diet.empty[Int]

    diet.show should be ("{ }")
  }

  test("shown all intervals"){
    val diet = Diet.empty[Int] + Range(1, 10) + Range(20, 100)

    diet.show should be ("{ [1, 10] [20, 100] }")
  }

  test("remove side ranges"){
    val diet = ((Diet.empty[Int]
      + Range(20, 21)
      + Range(9,10)
      + Range(12, 18)
      + Range(23, 30)
      + Range(40, 50)
      + Range(35, 48))
      - Range(15, 18)
      - Range(25, 60))

    diet.toList should be(List(9, 10, 12, 13, 14, 20, 21, 23, 24))
  }

  test("return empty when removing from empty"){
    (Diet.empty[Int] - Range(10, 100)) should be (Diet.EmptyDiet())
  }

  test("remove inner range"){
    val diet = ((Diet.empty[Int] + Range(20, 30)) - Range(22, 27))

    diet.toList should be(List(20, 21, 28, 29, 30))
  }

  test("insert/remove")(forAll { (rs: Ranges) =>
    rs.toDiet.toList should be(rs.toSet.toList.sorted)
  })

  test("distinct")(forAll { (rs: Ranges) =>
    val list = rs.toDiet.toList
    list.distinct should be(list)
  })

  test("min")(forAll { (rs: Ranges) =>
    whenever(!rs.isEmpty) {
      rs.toDiet.min should be(Some(rs.toSet.min))
    }
  })

  test("max")(forAll { (rs: Ranges) =>
    whenever(!rs.isEmpty) {
      rs.toDiet.max should be(Some(rs.toSet.max))
    }
  })

  test("merge")(forAll { (rs1: Ranges, rs2: Ranges) =>
    val diet1 = rs1.toDiet
    val diet2 = rs2.toDiet
    (diet1 ++ diet2).toList should be((rs1.toSet ++ rs2.toSet).toList.sorted)
  })

  test("join disjoint range"){
    val diet = Diet.empty[Int] + 5 + 6 + 7 + 1 + 2

    val other = diet + 3 + 4

    other.toList should be(List(1, 2, 3, 4, 5, 6, 7))
  }

}
/*
class DietTest extends DogsSuite {
  import Diet._



  test("contain items from range"){
    val diet = Diet.empty[Int].addRange(Range(5, 10)).addRange(Range(1, 3)).addRange(Range(12, 20))

    diet.contains(1) should be (true)
    diet.contains(2) should be (true)
    diet.contains(3) should be (true)

    diet.contains(4) should be (false)

    diet.contains(6) should be (true)

    diet.contains(15) should be (true)
  }

  test("return empty when removing from empty"){
    Diet.empty[Int].remove(1) should be (EmptyDiet())
  }

  test("not be modified when removing non existed item"){

    val diet = Diet.empty[Int] + 1 +2 + 3 + 5

    diet.remove(4) should be (diet)
  }

  test("be spl"){
    val diet = Diet.empty[Int] + 1 +2 + 3 + 5

    val other = diet.remove(2).intervals.map(x => x.generate)

    other should contain  inOrder (scala.List(1), scala.List(3), scala.List(5))
  }

  test("map"){
    val diet = Diet.empty[Int] + 1 +2 + 8 + 5

    val other = diet.map(x => x + 2).intervals.map(x => x.generate)

    other should matchTo(List[List[Int]](List(3,4), List(7), List(10)))
  }

  test("foldLeft"){
    val diet = Diet.empty[Int] + 1 +2 + 8 + 5

    diet.foldLeft(10)(_ + _) should be (26)
    diet.foldRight(10)(_ + _) should be (26)
  }

  test("contain range"){
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

class DietTestJoin extends DogsSuite {
  test("return the same diet when join to empty range"){
    val diet = Diet.empty[Int] + 20 + 30

    val range = Range.empty[Int]

    diet.addRange(range) should be (diet)
  }

  test("return a diet with range when added to empty diet"){
    val diet = Diet.empty[Int]

    val range = Range(20, 30)

    val other = diet.addRange(range)

    other.min should be (Some(20))
    other.max should be (Some(30))
  }

  test("increase range to the left"){
    val diet = Diet.empty[Int] + 20 + 21
    val range = Range(15, 19)

    val other = diet.addRange(range)

    other.intervals(0).generate should matchTo(List(15, 16, 17, 18, 19, 20, 21))
  }

  test("create disjoint range to the left"){
    val diet = Diet.empty[Int] + 20 + 21
    val range = Range(15, 18)

    val sets = diet.addRange(range).intervals.map(r=>r.generate)

    sets(0) should matchTo(List(15, 16, 17, 18))
    sets(1) should matchTo(List(20, 21))
  }

  test("increase range to the right"){
    val diet = Diet.empty[Int] + 20 + 22
    val range = Range(21, 30)

    val other = diet.addRange(range).intervals.map(r => r.generate)

    other should matchTo(List(Range(20, 30).toList))
  }

  test("join to an empty diet"){
    val diet = Diet.empty[Int] + Range(20, 30)

    val other = diet ++ Diet.empty[Int]

    other should be (diet)
  }

  test("join to another diet"){
    val diet = Diet.empty[Int] + Range(20, 30)

    val other = diet ++ (Diet.empty[Int] + Range(25, 35) + Range(5, 10) + Range(15, 22))

    val sets = other.intervals.map(r => r.generate)

    sets should matchTo(List(Range(5, 10).toList, Range(15, 35).toList))

    val otherSets = diet | other

    otherSets.intervals.map(r => r.generate) should matchTo(List(Range(5, 10).toList, Range(15, 35).toList))
  }

  test("intersect with another diet"){
    val diet = Diet.empty[Int] + Range(20, 30)

    (diet & Diet.empty[Int]).intervals.length should be (0)

    (diet & diet) should be(diet)

    (diet & (Diet.empty[Int] + Range(15, 25) + Range(28, 32))).toList should
      matchTo(List (20, 21, 22, 23, 24, 25, 28, 29, 30))

    (diet & (Diet.empty[Int] + Range(10, 15))).toList should matchTo(El[Int])
  }
}


 */
