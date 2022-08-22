/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
