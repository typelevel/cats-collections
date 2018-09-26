package cats.collections
package tests.arbitrary

import org.scalacheck.{Gen, Arbitrary}
import cats.Order


trait ArbitraryMap extends ArbitrarySet {

  def mapGen[K: Order: Arbitrary, A: Arbitrary]: Gen[AvlMap[K,A]] = {
    implicit def order[X](implicit K: Order[K]): Order[(K,X)] = Order.by[(K,X), K](_._1)(K)
    setGen[(K,A)].map(new AvlMap(_))
  }

  implicit def mapArbitrary[K: Arbitrary: Order, A: Arbitrary]: Arbitrary[AvlMap[K,A]] =
    Arbitrary(mapGen[K,A])
}
