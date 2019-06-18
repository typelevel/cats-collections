package cats.collections
package arbitrary

import org.scalacheck.{Gen, Arbitrary}
import cats.Order

trait ArbitraryPredicate {
  import set._
  def predicateGen[A: Arbitrary: Order]: Gen[Predicate[A]] =
    setGen.map(_.predicate)

  implicit def predicateArbitrary[A: Arbitrary: Order]: Arbitrary[Predicate[A]] =
    Arbitrary(predicateGen[A])
}

