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
    Gen.sized {
      case 0 => Gen.const(BList.empty)
      case n =>
        Gen.oneOf(
          // empty
          Gen.const(BList.empty),
          // prepend
          for {
            hs <- Gen.resize(n / 2, bListGen[A])
            a <- arbitrary[A]
          } yield hs.prepend(a),
          // concat
          for {
            xs <- Gen.resize(n / 4, bListGen[A])
            ys <- Gen.resize(n / 4, bListGen[A])
          } yield xs.concat(ys),
          // map
          Gen.resize(n / 2, bListGen[A]).flatMap { l =>
            arbitrary[A => A].map(fn => l.map(fn))
          }
        )
    }
  }

  implicit def arbitraryBList[A](implicit arb: Arbitrary[A], cogen: Cogen[A]): Arbitrary[BList[A]] =
    Arbitrary(bListGen(arb, cogen))
}

object ArbitraryBList extends ArbitraryBList
