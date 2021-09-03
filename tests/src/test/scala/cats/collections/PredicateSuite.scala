package cats.collections

import cats._
import cats.collections.arbitrary.predicate._
import cats.laws.discipline.{ContravariantMonoidalTests, SerializableTests}
import cats.syntax.eq._
import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.scalacheck.Test

class PredicateSuite extends DisciplineSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default

  checkAll("Monoid[Predicate[Int]]", SerializableTests.serializable(Monoid[Predicate[Int]]))
  checkAll("MonoidK[Predicate]", SerializableTests.serializable(MonoidK[Predicate]))
  checkAll("Serializable[ContravariantMonoidal[Predicate]]", SerializableTests.serializable(ContravariantMonoidal[Predicate]))

  {
    implicit val eqForPredicateInt: Eq[Predicate[Int]] = new Eq[Predicate[Int]] {
      override def eqv(x: Predicate[Int], y: Predicate[Int]): Boolean = x(0) === y(0)
    }
    implicit val eqForPredicateTripleInt: Eq[Predicate[(Int, Int, Int)]] = new Eq[Predicate[(Int, Int, Int)]] {
      override def eqv(x: Predicate[(Int, Int, Int)], y: Predicate[(Int, Int, Int)]): Boolean = x((0, 0, 0)) === y((0, 0, 0))
    }
    checkAll("ContravariantMonoidal[Predicate]", ContravariantMonoidalTests[Predicate].contravariantMonoidal[Int, Int, Int])
  }

  property("intersection works")(
    forAll { (as: List[Int], bs: List[Int]) =>

      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA & setEven
      val s2 = setA intersection setEven

      assert(bs.forall(b => (s1(b) == (as.contains(b) && (b % 2 == 0))) &&
                  (s2(b) == (as.contains(b) && (b % 2 == 0)))))
    })

  property("union works")(
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA | setEven
      val s2 = setA union setEven

      assert(bs.forall(b => (s1(b) == (as.contains(b) || (b % 2 == 0))) &&
                  (s2(b) == (as.contains(b) || (b % 2 == 0)))))
    })

  property("difference works") (
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA - setEven
      val s2 = setA diff setEven

      assert(bs.forall(b => (s1(b) == (as.contains(b) && (b % 2 != 0))) &&
                  (s2(b) == (as.contains(b) && (b % 2 != 0)))))
    })

  property("negation works")(
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = !(setA - setEven)

      assert(bs.forall(b => (s1(b) != (as.contains(b) && (b % 2 != 0)))))
    })
}
