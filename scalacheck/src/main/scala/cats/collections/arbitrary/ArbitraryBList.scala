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

import cats.collections.BList
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbitraryBList {
  def bListGen[A](implicit arb: Arbitrary[A], cogen: Cogen[A]): Gen[BList[A]] = {
    // implicit lazy val arbBList: Arbitrary[BList[A]] = arbitraryBList[A]

    Gen.oneOf(
      // empty
      Gen.const(BList.empty),
      // from List
      // arbitrary[List[A]].map(BList.fromList(_)),
      // prepend
      Gen.delay(for {
        hs <- arbitrary[BList[A]]
        a <- arbitrary[A]
      } yield hs.prepend(a)),
      // concat
      Gen.delay(for {
        ls <- arbitrary[BList[A]]
        xs <- arbitrary[BList[A]]
      } yield ls.concat(xs)),
      // map
      Gen.delay(for {
        l <- arbitrary[BList[A]]
        fn <- arbitrary[A => A]
      } yield l.map(fn))
    )
  }

  implicit def arbitraryBList[A](implicit arb: Arbitrary[A], cogen: Cogen[A]): Arbitrary[BList[A]] =
    Arbitrary(bListGen(arb, cogen))
}

object ArbitraryBList extends ArbitraryBList
