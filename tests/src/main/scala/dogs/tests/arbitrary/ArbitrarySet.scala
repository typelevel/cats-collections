package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}, Arbitrary._
import cats.Order

trait ArbitrarySet {

  def setGen[A: Order: Arbitrary]: Gen[Set[A]] =
    arbitrary[List[A]].map(Set.fromList[A])

  implicit def setArbitrary[A: Arbitrary: Order]: Arbitrary[Set[A]] =
    Arbitrary(setGen[A])
}

