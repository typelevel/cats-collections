package cats.collections
package arbitrary

import org.scalacheck.{Arbitrary, Gen}
import cats.Order

trait ArbitraryMap {
  import set._

  def mapGen[K: Order: Arbitrary, A: Arbitrary]: Gen[AvlMap[K, A]] = {
    implicit def order[X](implicit K: Order[K]): Order[(K, X)] = Order.by[(K, X), K](_._1)(K)
    setGen[(K, A)].map(new AvlMap(_))
  }

  implicit def mapArbitrary[K: Arbitrary: Order, A: Arbitrary]: Arbitrary[AvlMap[K, A]] =
    Arbitrary(mapGen[K, A])
}
