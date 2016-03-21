package dogs
package tests

import cats.Eq
import org.scalatest.{FunSuite, PropSpec, Matchers}
import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}

trait DogsSuite extends FunSuite
    with Matchers
    with Configuration
    with GeneratorDrivenPropertyChecks
    with Discipline
    with DogMatcher {

  implicit def eqTuple3[A: Eq, B: Eq, C: Eq](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[(A,B,C)] = new Eq[(A,B,C)] {
    def eqv(l: (A,B,C), r: (A,B,C)) =
      A.eqv(l._1, r._1) &&
      B.eqv(l._2, r._2) &&
      C.eqv(l._3, r._3)
  }

  implicit def eqTuple2[A: Eq, B: Eq](implicit A: Eq[A], B: Eq[B]): Eq[(A,B)] = new Eq[(A,B)] {
    def eqv(l: (A,B), r: (A,B)) =
      A.eqv(l._1, r._1) &&
      B.eqv(l._2, r._2)
  }
}
