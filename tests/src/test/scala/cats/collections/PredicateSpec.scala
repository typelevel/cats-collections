package cats.collections
package tests

import cats.collections.arbitrary.predicate._
import cats.laws.discipline.{ContravariantMonoidalTests, SerializableTests}
import cats._
import cats.instances.int._
import cats.tests.CatsSuite

class PredicateSpec extends CatsSuite {
  checkAll("Monoid[Predicate[Int]]", SerializableTests.serializable(Monoid[Predicate[Int]]))
  checkAll("MonoidK[Predicate]", SerializableTests.serializable(MonoidK[Predicate]))
  checkAll("Serializable[ContravariantMonoidal[Predicate]]", SerializableTests.serializable(ContravariantMonoidal[Predicate]))

  implicit val eqForPredicateInt: Eq[Predicate[Int]] = new Eq[Predicate[Int]] {
    override def eqv(x: Predicate[Int], y: Predicate[Int]): Boolean = x(0) === y(0)
  }
  implicit val eqForPredicateTripleInt: Eq[Predicate[(Int, Int, Int)]] = new Eq[Predicate[(Int, Int, Int)]] {
    override def eqv(x: Predicate[(Int, Int, Int)], y: Predicate[(Int, Int, Int)]): Boolean = x((0, 0, 0)) === y((0, 0, 0))
  }
  checkAll("ContravariantMonoidal[Predicate]", ContravariantMonoidalTests[Predicate].contravariantMonoidal[Int, Int, Int])

  test("intersection works")(
    forAll { (as: List[Int], bs: List[Int]) =>

      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA & setEven
      val s2 = setA intersection setEven

      bs.forall(b => (s1(b) == (as.contains(b) && (b % 2 == 0))) &&
                  (s2(b) == (as.contains(b) && (b % 2 == 0)))) should be(true)

    })

  test("union works")(
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA | setEven
      val s2 = setA union setEven

      bs.forall(b => (s1(b) == (as.contains(b) || (b % 2 == 0))) &&
                  (s2(b) == (as.contains(b) || (b % 2 == 0)))) should be (true)

    })

  test("difference works") (
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = setA - setEven
      val s2 = setA diff setEven

      bs.forall(b => (s1(b) == (as.contains(b) && (b % 2 != 0))) &&
                  (s2(b) == (as.contains(b) && (b % 2 != 0)))) should be (true)

    })

  test("negation works")(
    forAll { (as: List[Int], bs: List[Int]) =>
      val setA: Predicate[Int] = AvlSet.fromList(as).predicate
      val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

      val s1 = !(setA - setEven)

      bs.forall(b => (s1(b) != (as.contains(b) && (b % 2 != 0)))) should be(true)

    })
}
