package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}, Arbitrary.arbitrary

trait ArbitraryOption {
  def optionGen[A](implicit A: Arbitrary[A]): Gen[Option[A]] =
    arbitrary[scala.Option[A]].map(Option.fromScalaOption)

  implicit def arbitraryOption[A](implicit A: Arbitrary[A]): Arbitrary[Option[A]] =
    Arbitrary(optionGen(A))
}
