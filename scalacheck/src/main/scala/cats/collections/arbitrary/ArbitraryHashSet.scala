package cats.collections.arbitrary

import cats.collections.HashSet
import cats.kernel.Hash
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import scala.collection.immutable.Seq

trait ArbitraryHashSet {
  def hashSetGen[A: Hash: Arbitrary: Cogen]: Gen[HashSet[A]] =
    Gen.oneOf(
      // empty
      Gen.const(HashSet.empty),
      // fromSeq
      arbitrary[Seq[A]].map(HashSet.fromSeq(_)),
      // fromIterableOnce
      arbitrary[Seq[A]].map(seq => HashSet.fromIterableOnce(seq.view)),
      // fromFoldable
      arbitrary[Seq[A]].map(HashSet.fromFoldable(_)),
      // add
      Gen.delay(for {
        hs <- arbitrary[HashSet[A]]
        a <- arbitrary[A]
      } yield hs.add(a)),
      // add existing
      Gen.delay(for {
        hs <- arbitrary[HashSet[A]]
        if hs.nonEmpty
        a <- Gen.oneOf(hs.iterator.toList)
      } yield hs.add(a)),
      // remove
      Gen.delay(for {
        hs <- arbitrary[HashSet[A]]
        a <- arbitrary[A]
      } yield hs.remove(a)),
      // remove existing
      Gen.delay(for {
        hs <- arbitrary[HashSet[A]]
        if hs.nonEmpty
        a <- Gen.oneOf(hs.iterator.toList)
      } yield hs.remove(a)),
      // union
      Gen.delay(for {
        left <- arbitrary[HashSet[A]]
        right <- arbitrary[HashSet[A]]
      } yield left.union(right)),
      // diff
      Gen.delay(for {
        left <- arbitrary[HashSet[A]]
        right <- arbitrary[HashSet[A]]
      } yield left.diff(right)),
      // intersect
      Gen.delay(for {
        left <- arbitrary[HashSet[A]]
        right <- arbitrary[HashSet[A]]
      } yield left.intersect(right)),
      // filter
      Gen.delay(for {
        set <- arbitrary[HashSet[A]]
        pred <- arbitrary[A => Boolean]
      } yield set.filter(pred)),
      // filterNot
      Gen.delay(for {
        set <- arbitrary[HashSet[A]]
        pred <- arbitrary[A => Boolean]
      } yield set.filterNot(pred))
    )

  implicit def hashSetCogen[A](implicit A: Cogen[A]): Cogen[HashSet[A]] =
    Cogen.it[HashSet[A], A](_.iterator)

  implicit def hashSetArbitrary[A: Hash: Arbitrary: Cogen]: Arbitrary[HashSet[A]] =
    Arbitrary(hashSetGen)
}
