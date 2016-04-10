package dogs
package tests.arbitrary

import Predef._
import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary
import cats.Order

trait ArbitraryISet {
  import set._
  def isetGen[A: Arbitrary: Order]: Gen[ISet[A]] =
    setGen.map(_.iset)

  implicit def isetArbitrary[A: Arbitrary: Order]: Arbitrary[ISet[A]] =
    Arbitrary(isetGen[A])
}

