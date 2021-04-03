package cats.collections
package arbitrary

import org.scalacheck.{Gen, Cogen, Arbitrary}

trait ArbitraryPredicate {
  import Gen._
  def predicateGen[A: Arbitrary: Cogen]: Gen[Predicate[A]] =
    oneOf(const(Predicate.empty), const(Predicate.everything), resultOf(Predicate(_: A => Boolean)))

  implicit def predicateArbitrary[A: Arbitrary: Cogen]: Arbitrary[Predicate[A]] =
    Arbitrary(predicateGen[A])
}
