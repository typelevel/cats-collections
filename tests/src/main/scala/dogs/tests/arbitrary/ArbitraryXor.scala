package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary
import Gen.oneOf

trait ArbitraryXor {
  def genXor[A,B](implicit A: Arbitrary[A], B: Arbitrary[B]): Gen[A Xor B] =
    Gen.oneOf(A.arbitrary.map(Xor.left), B.arbitrary.map(Xor.right))

  implicit def arbitraryXor[A: Arbitrary, B: Arbitrary]: Arbitrary[A Xor B] =
    Arbitrary(genXor[A,B])
}
