package cats.collections.arbitrary

import org.scalacheck.Arbitrary
import cats.kernel.Hash
import org.scalacheck.Gen
import cats.collections.HashMap
import scala.collection.immutable.Seq
import org.scalacheck.Cogen

trait ArbitraryHashMap {
  def hashMapGen[K, V](implicit
    K: Arbitrary[K],
    V: Arbitrary[V],
    hash: Hash[K]
  ): Gen[HashMap[K, V]] =
    Gen.oneOf(
      // empty
      Gen.const(HashMap.empty),
      // fromSeq
      Arbitrary.arbitrary[Seq[(K, V)]].map(HashMap.fromSeq(_)(hash)),
      // fromIterableOnce
      Arbitrary.arbitrary[Seq[(K, V)]].map(seq => HashMap.fromIterableOnce(seq.view)),
      // fromFoldable
      Arbitrary.arbitrary[Seq[(K, V)]].map(HashMap.fromFoldable(_)),
      // updated
      Gen.delay(for {
        hm <- Arbitrary.arbitrary[HashMap[K, V]]
        (k, v) <- Arbitrary.arbitrary[(K, V)]
      } yield hm.updated(k, v)),
      // updated existing
      Gen.delay(for {
        hm <- Arbitrary.arbitrary[HashMap[K, V]]
        if hm.nonEmpty
        k <- Gen.oneOf(hm.keysIterator.toList)
        v <- Arbitrary.arbitrary[V]
      } yield hm.updated(k, v)),
      // removed
      Gen.delay(for {
        hm <- Arbitrary.arbitrary[HashMap[K, V]]
        k <- Arbitrary.arbitrary[K]
      } yield hm.removed(k)),
      // removed existing
      Gen.delay(for {
        hm <- Arbitrary.arbitrary[HashMap[K, V]]
        if hm.nonEmpty
        k <- Gen.oneOf(hm.keysIterator.toList)
      } yield hm.removed(k)),
      // concat
      Gen.delay(for {
        left <- Arbitrary.arbitrary[HashMap[K, V]]
        right <- Arbitrary.arbitrary[HashMap[K, V]]
      } yield left.concat(right))
    )

  implicit def hashMapCogen[K, V](implicit K: Cogen[K], V: Cogen[V]): Cogen[HashMap[K, V]] =
    Cogen.it[HashMap[K, V], (K, V)](_.iterator)

  implicit def hashMapArbitrary[K: Hash: Arbitrary, V: Arbitrary]: Arbitrary[HashMap[K, V]] =
    Arbitrary(hashMapGen[K, V])
}
