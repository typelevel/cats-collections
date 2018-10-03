package cats.collections
package arbitrary

import org.scalacheck.{Gen, Arbitrary}
import cats.Order

trait ArbitraryISet {
  import set._
  def isetGen[A: Arbitrary: Order]: Gen[ISet[A]] =
    setGen.map(_.iset)

  implicit def isetArbitrary[A: Arbitrary: Order]: Arbitrary[ISet[A]] =
    Arbitrary(isetGen[A])
}

