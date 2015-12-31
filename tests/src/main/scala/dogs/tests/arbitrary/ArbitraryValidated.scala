package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary
import Gen.oneOf

trait ArbitraryValidated {
  def genValidated[A,B](implicit A: Arbitrary[A], B: Arbitrary[B]): Gen[Validated[A,B]] =
    Gen.oneOf(A.arbitrary.map(Validated.invalid), B.arbitrary.map(Validated.valid))

  implicit def arbitraryValidated[A: Arbitrary, B: Arbitrary]: Arbitrary[Validated[A, B]] =
    Arbitrary(genValidated[A,B])
}
