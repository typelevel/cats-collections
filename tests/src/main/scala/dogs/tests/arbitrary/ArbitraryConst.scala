package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}, Arbitrary.arbitrary

trait ArbitraryConst {
  implicit def constArbitrary[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))
}
