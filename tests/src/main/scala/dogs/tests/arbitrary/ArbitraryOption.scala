package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}, Arbitrary.arbitrary

trait ArbitraryOption {
  def optionGen[A](implicit A: Arbitrary[A]): Gen[Option[A]] =
    arbitrary[scala.Option[A]].map(Option.fromStdOption)

  implicit def arbitraryOption[A: Arbitrary]: Arbitrary[Option[A]] =
    Arbitrary(optionGen)
}
