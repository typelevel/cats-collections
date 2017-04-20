package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary

trait ArbitrarySet {
  def setGen[A: Order: Arbitrary]: Gen[Set[A]] =
    arbitrary[List[A]].map(Set.fromList[A])

  implicit def setArbitrary[A: Arbitrary: Order]: Arbitrary[Set[A]] =
    Arbitrary(setGen[A])
}

