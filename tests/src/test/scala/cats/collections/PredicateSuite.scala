/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
  checkAll("Serializable[ContravariantMonoidal[Predicate]]",
           SerializableTests.serializable(ContravariantMonoidal[Predicate])
  )

  {
    implicit val eqForPredicateInt: Eq[Predicate[Int]] = new Eq[Predicate[Int]] {
      override def eqv(x: Predicate[Int], y: Predicate[Int]): Boolean = x(0) === y(0)
    }
    implicit val eqForPredicateTripleInt: Eq[Predicate[(Int, Int, Int)]] = new Eq[Predicate[(Int, Int, Int)]] {
      override def eqv(x: Predicate[(Int, Int, Int)], y: Predicate[(Int, Int, Int)]): Boolean =
        x((0, 0, 0)) === y((0, 0, 0))
    }
    checkAll("ContravariantMonoidal[Predicate]",
             ContravariantMonoidalTests[Predicate].contravariantMonoidal[Int, Int, Int]
    )
  }

  property("intersection works")(forAll { (as: List[Int], bs: List[Int]) =>

    val setA: Predicate[Int] = AvlSet.fromList(as).predicate
    val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

    val s1 = setA & setEven
    val s2 = setA.intersection(setEven)

    assert(
      bs.forall(b =>
        (s1(b) == (as.contains(b) && (b % 2 == 0))) &&
          (s2(b) == (as.contains(b) && (b % 2 == 0)))
      )
    )
  })

  property("union works")(forAll { (as: List[Int], bs: List[Int]) =>
    val setA: Predicate[Int] = AvlSet.fromList(as).predicate
    val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

    val s1 = setA | setEven
    val s2 = setA.union(setEven)

    assert(
      bs.forall(b =>
        (s1(b) == (as.contains(b) || (b % 2 == 0))) &&
          (s2(b) == (as.contains(b) || (b % 2 == 0)))
      )
    )
  })

  property("difference works")(forAll { (as: List[Int], bs: List[Int]) =>
    val setA: Predicate[Int] = AvlSet.fromList(as).predicate
    val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

    val s1 = setA - setEven
    val s2 = setA.diff(setEven)

    assert(
      bs.forall(b =>
        (s1(b) == (as.contains(b) && (b % 2 != 0))) &&
          (s2(b) == (as.contains(b) && (b % 2 != 0)))
      )
    )
  })

  property("negation works")(forAll { (as: List[Int], bs: List[Int]) =>
    val setA: Predicate[Int] = AvlSet.fromList(as).predicate
    val setEven: Predicate[Int] = Predicate(_ % 2 == 0)

    val s1 = !(setA - setEven)

    assert(bs.forall(b => s1(b) != (as.contains(b) && (b % 2 != 0))))
  })
}
