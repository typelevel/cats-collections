package cats.collections
package arbitrary

import org.scalacheck.{Arbitrary, Gen}, Arbitrary._
import cats.Order

trait ArbitrarySet {

  def setGen[A: Order: Arbitrary]: Gen[AvlSet[A]] =
    arbitrary[List[A]].map(AvlSet.fromList[A])

  implicit def setArbitrary[A: Arbitrary: Order]: Arbitrary[AvlSet[A]] =
    Arbitrary(setGen[A])
}
