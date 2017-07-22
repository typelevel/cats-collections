package dogs
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import cats.Order

trait ArbitraryISet {
  import set._
  def isetGen[A](implicit arb: Arbitrary[A], ord: Order[A]): Gen[ISet[A]] =
    setGen(ord,arb).map(_.iset)

  implicit def isetArbitrary[A: Arbitrary: Order]: Arbitrary[ISet[A]] =
    Arbitrary(isetGen[A])
}

