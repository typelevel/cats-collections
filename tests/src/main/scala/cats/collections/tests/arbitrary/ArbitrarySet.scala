package cats.collections
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}, Arbitrary._
import cats.Order

trait ArbitrarySet {

  def setGen[A: Order: Arbitrary]: Gen[AvlSet[A]] =
    arbitrary[List[A]].map(AvlSet.fromList[A])

  implicit def setArbitrary[A: Arbitrary: Order]: Arbitrary[AvlSet[A]] =
    Arbitrary(setGen[A])
}

