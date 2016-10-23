package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import cats.Order

trait ArbitrarySet {

  import list._
  def setGen[A: Order: Arbitrary]: Gen[Set[A]] =
    listGen[A].map(Set.fromList[A])

  implicit def setArbitrary[A: Arbitrary: Order]: Arbitrary[Set[A]] =
    Arbitrary(setGen[A])
}

