package cats.collections

import algebra.laws._
import cats._
import cats.syntax.show._
import cats.kernel.laws.discipline._
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Prop._

import scala.util.Random

object DietSuite {
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
      def toRange: Range[Int] = Range(x, y)

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
        Gen.choose(0, 999).map(Item.Single.apply),
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
      Gen
        .listOf {
          for {
            inout <- Gen.oneOf(true, false)
            item <- Arbitrary.arbitrary[Item]
          } yield (inout, item)
        }
        .map(Ranges.apply)
    )

  implicit val arbDiet: Arbitrary[Diet[Int]] = Arbitrary(arbRanges.arbitrary.map(_.toDiet))

  implicit def dietEq: Eq[Diet[Int]] = Diet.eqDiet
}

class DietSuite extends DisciplineSuite {
  import DietSuite._

  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default
      .withMinSuccessfulTests(if (BuildInfo.isJvm) 5000 else 500)

  test("shown empty") {
    val diet = Diet.empty[Int]

    assertEquals(diet.show, "Diet( )")
  }

  test("shown all intervals") {
    val diet = Diet.empty[Int] + Range(1, 10) + Range(20, 100)

    assertEquals(diet.show, "Diet( [1, 10] [20, 100] )")
  }

  test("remove side ranges") {
    val diet = ((Diet.empty[Int]
      + Range(20, 21)
      + Range(9, 10)
      + Range(12, 18)
      + Range(23, 30)
      + Range(40, 50)
      + Range(35, 48))
      - Range(15, 18)
      - Range(25, 60))

    assertEquals(diet.toList, List(9, 10, 12, 13, 14, 20, 21, 23, 24))
  }

  test("return empty when removing from empty") {
    assertEquals(Diet.empty[Int] - Range(10, 100), Diet.EmptyDiet[Int]())
  }

  test("diet eq") {
    val diet = (1 to 100).filter(_ % 2 == 0).foldLeft(Diet.empty[Int])(_ add _)
    val inverted = (1 to 100).reverse.filter(_ % 2 == 0).foldLeft(Diet.empty[Int])(_ add _)

    assert(dietEq.eqv(diet, inverted))
  }

  property("same ranges define the same diet")(forAll { (rs: Ranges) =>
    val ranges = rs.rs.map(_._2)
    val reversed = rs.rs.reverse.map(_._2)

    val d1 = ranges.foldLeft(Diet.empty[Int]) { (diet, r) => r.addToDiet(diet) }
    val d2 = reversed.foldLeft(Diet.empty[Int]) { (diet, r) => r.addToDiet(diet) }

    assert(dietEq.eqv(d1, d2))
  })

  property("reshaping results on the same diet")(forAll { (rs: Ranges) =>

    val d1 = rs.rs.map(_._2).foldLeft(Diet.empty[Int]) { (diet, r) => r.addToDiet(diet) }

    val ranges = Random.shuffle(rs.rs)

    val d2 = ranges.map(_._2).foldLeft(Diet.empty[Int]) { (diet, r) => r.addToDiet(diet) }

    assert(dietEq.eqv(d1, d2))
  })

  property("different set of ranges ==> different diets")(forAll { (a: Ranges, b: Ranges) =>
    if (a.toSet.size != b.toSet.size) {
      assertEquals(dietEq.eqv(a.toDiet, b.toDiet), false)
    }
  })

  test("remove inner range") {
    val diet = (Diet.empty[Int] + Range(20, 30)) - Range(22, 27)

    assertEquals(diet.toList, List(20, 21, 28, 29, 30))
  }

  property("insert/remove")(forAll { (rs: Ranges) =>
    assertEquals(rs.toDiet.toList, rs.toSet.toList.sorted)
  })

  property("distinct")(forAll { (rs: Ranges) =>
    val list = rs.toDiet.toList
    assertEquals(list.distinct, list)
  })

  property("min")(forAll { (rs: Ranges) =>
    if (!rs.isEmpty) {
      assertEquals(rs.toDiet.min, Some(rs.toSet.min))
    }
  })

  property("max")(forAll { (rs: Ranges) =>
    if (!rs.isEmpty) {
      assertEquals(rs.toDiet.max, Some(rs.toSet.max))
    }
  })

  test("min/max empty") {
    assertEquals(Diet.empty[Int].min, None)
    assertEquals(Diet.empty[Int].max, None)
  }

  property("merge")(forAll { (rs1: Ranges, rs2: Ranges) =>
    val diet1 = rs1.toDiet
    val diet2 = rs2.toDiet

    assertEquals((diet1 ++ diet2).toList, (rs1.toSet ++ rs2.toSet).toList.sorted)
  })

  property("intersection range")(forAll { (rs: Ranges, m: Int, n: Int) =>
    if (m >= n) {
      val diet = rs.toDiet
      val r = Range(n, m)

      assertEquals((diet & r).toList, diet.toList.filter(r.contains))
    }
  })

  property("intersection diet")(forAll { (rs1: Ranges, rs2: Ranges) =>
    assertEquals((rs1.toDiet & rs2.toDiet).toList, rs1.toSet.intersect(rs2.toSet).toList.sorted)
  })

  test("join disjoint range") {
    val diet = Diet.empty[Int] + 5 + 6 + 7 + 1 + 2

    val other = diet + 3 + 4

    assertEquals(other.toList, List(1, 2, 3, 4, 5, 6, 7))
  }

  property("contains")(forAll { (rs: Ranges) =>
    val diet = rs.toDiet
    val set = rs.toSet

    set.foreach(elem => assert(diet.contains(elem)))
  })

  property("not contains")(forAll { (rs: Ranges, elem: Int) =>
    val diet = rs.toDiet
    val set = rs.toSet

    if (!set.contains(elem)) {
      assert(!diet.contains(elem))
    }
  })

  property("not be modified when removing non-existent item")(forAll { (d: Diet[Int], elem: Int) =>
    if (!d.contains(elem)) {
      assert(d.remove(elem) == d)
    }
  })

  def invariant[A](d: Diet[A])(implicit order: Order[A], discrete: Discrete[A]): Boolean = d match {
    case Diet.DietNode(rng, left, right) =>
      order.lteqv(rng.start, rng.end) &&
      left.toList.forall(order.lt(_, discrete.pred(rng.start))) &&
      right.toList.forall(order.gt(_, discrete.succ(rng.end))) &&
      invariant(left) &&
      invariant(right)
    case _ =>
      true
  }

  test("invariant regression") {
    val diet = Diet.empty[Int] + Range(1, 3) - 2 + 2
    assert(invariant(diet))
  }

  property("invariant")(forAll { (rs: Ranges) =>
    assert(invariant(rs.toDiet))
  })

  property("one and fromRange are consistent")(forAll { (x: Int) =>
    assertEquals(Diet.fromRange(Range(x, x)), Diet.one(x))
  })

  property("fromRange contains consistent with Range contains")(forAll { (x: Byte, y: Byte, z: Byte) =>
    val range = if (x < y) Range(x, y) else Range(y, x)
    assertEquals(Diet.fromRange(range).contains(z), range.contains(z))
  })

  property("one and contains are consistent")(forAll { (x: Int, y: Int) =>
    assertEquals(Diet.one(x).contains(y), x == y)
  })

  property("one toList")(forAll { (x: Int) =>
    assertEquals(Diet.one(x).toList, x :: Nil)
  })

  property("range toList")(forAll { (x: Byte, y: Byte) =>
    val range = if (x < y) Range(x, y) else Range(y, x)
    assertEquals(Diet.fromRange(range).toList, range.toList)
  })

  checkAll("Diet[Int]", CommutativeMonoidTests[Diet[Int]].commutativeMonoid)
  checkAll("Diet[Int]", RingLaws[Diet[Int]].semiring)
  checkAll("Diet[Int]", LogicLaws[Diet[Int]].generalizedBool)

}
