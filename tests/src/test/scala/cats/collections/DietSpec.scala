package cats.collections
package tests

import catalysts.Platform
import cats._
import cats.collections.arbitrary.cogen._
import org.scalacheck._
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import algebra.laws._

class DietSpec extends CatsSuite {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration.copy(
      minSuccessful = if (Platform.isJvm) 10000 else 500
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

  implicit val arbDiet: Arbitrary[Diet[Int]] = Arbitrary(arbRanges.arbitrary.map(_.toDiet))

  // TODO requires a reasonable implementation!
  implicit def dietEq: Eq[Diet[Int]] = Eq.instance((x, y) => x.toList == y.toList)

  test("shown empty"){
    val diet = Diet.empty[Int]

    diet.show should be ("Diet( )")
  }

  test("shown all intervals"){
    val diet = Diet.empty[Int] + Range(1, 10) + Range(20, 100)

    diet.show should be ("Diet( [1, 10] [20, 100] )")
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

  test("min/max empty") {
    Diet.empty[Int].min should be(None)
    Diet.empty[Int].max should be(None)
  }

  test("foldLeft")(forAll { (rs: Ranges, start: Int, f: (Int, Int) => Int) =>
    rs.toDiet.foldLeft(start)(f) should be(rs.toSet.toList.sorted.foldLeft(start)(f))
  })

  test("foldLeft/toList")(forAll { (rs: Ranges) =>
    rs.toDiet.foldLeft(List.empty[Int])(_ :+ _) should be(rs.toDiet.toList)
  })

  test("foldRight")(forAll { (rs: Ranges, start: Int, f: (Int, Int) => Int) =>
    rs.toDiet.foldRight(Eval.now(start))((v, acc) => acc.map(f(v, _))).value should be(rs.toSet.toList.sorted.foldRight(start)(f))
  })

  test("foldRight/toList")(forAll { (rs: Ranges) =>
    rs.toDiet.foldRight(Eval.now(List.empty[Int]))((v, acc) => acc.map(v :: _)).value should be(rs.toDiet.toList)
  })

  test("merge")(forAll { (rs1: Ranges, rs2: Ranges) =>
    val diet1 = rs1.toDiet
    val diet2 = rs2.toDiet
    (diet1 ++ diet2).toList should be((rs1.toSet ++ rs2.toSet).toList.sorted)
  })

  test("intersection range")(forAll { (rs: Ranges, m: Int, n: Int) =>
    whenever(m >= n) {
      val diet = rs.toDiet
      val r = Range(n, m)
      (diet & r).toList should be(diet.toList.filter(r.contains))
    }
  })

  test("intersection diet")(forAll { (rs1: Ranges, rs2: Ranges) =>
    (rs1.toDiet & rs2.toDiet).toList should be((rs1.toSet intersect rs2.toSet).toList.sorted)
  })

  test("join disjoint range"){
    val diet = Diet.empty[Int] + 5 + 6 + 7 + 1 + 2

    val other = diet + 3 + 4

    other.toList should be(List(1, 2, 3, 4, 5, 6, 7))
  }

  test("contains")(forAll { (rs: Ranges) =>
    val diet = rs.toDiet
    val set = rs.toSet
    set.foreach(elem =>
      assert(diet.contains(elem))
    )
  })

  test("not contains")(forAll { (rs: Ranges, elem: Int) =>
    val diet = rs.toDiet
    val set = rs.toSet
    whenever(!set.contains(elem)) {
      assert(!diet.contains(elem))
    }
  })

  test("not be modified when removing non-existent item")(forAll { (d: Diet[Int], elem: Int) =>
    whenever(!d.contains(elem)) {
      assert(d.remove(elem) == d)
    }
  })

  {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration =
      checkConfiguration.copy(minSuccessful = 300)

    test("not be modified when inserting existing item")(forAll { (d: Diet[Int]) =>
      d.toList.foreach(elem =>
        // there may be structural changes, so fall back to list comparison
        d.add(elem).toList should be(d.toList)
      )
    })

    test("--")(forAll { (d1: Diet[Int], d2: Diet[Int]) =>
      val d = d1 -- d2
      d2.toList.foreach(elem => assert(!d.contains(elem)))
      d1.toList.foreach(elem => assert(d2.contains(elem) || d.contains(elem)))
    })
  }

  def invariant[A](d: Diet[A])(implicit order: Order[A], enum: Discrete[A]): Boolean = d match {
    case Diet.DietNode(rng, left, right) =>
      order.lteqv(rng.start, rng.end) &&
        left.toList.forall(order.lt(_, enum.pred(rng.start))) &&
        right.toList.forall(order.gt(_, enum.succ(rng.end))) &&
        invariant(left) &&
        invariant(right)
    case _ =>
      true
  }

  test("invariant regression") {
    val diet = Diet.empty[Int] + Range(1, 3) - 2 + 2
    assert(invariant(diet))
  }

  test("invariant")(forAll { (rs: Ranges) =>
    assert(invariant(rs.toDiet))
  })

  checkAll("Diet[Int]", CommutativeMonoidTests[Diet[Int]].commutativeMonoid)
  checkAll("Diet[Int]", RingLaws[Diet[Int]].semiring)
  checkAll("Diet[Int]", LogicLaws[Diet[Int]].generalizedBool)

}
