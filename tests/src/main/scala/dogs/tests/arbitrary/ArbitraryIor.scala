package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary
import Gen.oneOf

trait ArbitraryIor {
  def genIor[A,B](implicit A: Arbitrary[A], B: Arbitrary[B]): Gen[A Ior B] =
    Gen.oneOf(A.arbitrary.map(Ior.left),
              B.arbitrary.map(Ior.right),
              for {
                a <- A.arbitrary
                b <- B.arbitrary
              } yield(Ior.both(a,b)))

  implicit def arbitraryIor[A: Arbitrary, B: Arbitrary]: Arbitrary[A Ior B] =
    Arbitrary(genIor[A,B])
}
