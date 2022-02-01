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

package cats.collections.laws.discipline

import cats.collections.laws.PartiallyOrderedSetLaws
import cats.collections.PartiallyOrderedSet
import cats.kernel.{CommutativeMonoid, Eq, Order}
import cats.kernel.laws.discipline.OrderTests
import cats.laws.discipline.{catsLawsIsEqToProp, UnorderedFoldableTests}
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop.forAll

import scala.annotation.nowarn

trait PartiallyOrderedSetTests[F[_]] extends UnorderedFoldableTests[F] {
  override def laws: PartiallyOrderedSetLaws[F]

  def partiallyOrderedSet[A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbF: Arbitrary[A => B],
    ArbFAFA: Arbitrary[F[A] => F[A]],
    ArbBAB: Arbitrary[(B, A) => B],
    CogenA: Cogen[A],
    A: CommutativeMonoid[A],
    B: CommutativeMonoid[B],
    OrdFA: Order[A],
    EqFB: Eq[B]
  ): RuleSet = {
    implicit val ordFA: Order[F[A]] = laws.F.order[A]
    new RuleSet {
      val name = "partiallyOrderedSet"
      val bases = Nil
      val parents = List(unorderedFoldable[A, B])

      @nowarn val unaddMatchesProxyProps = forAll(laws.unaddMatchesProxy[A] _)

      val props = List(
        "size matches unorderedFoldMap" -> forAll(laws.sizeMatchesUnorderedFoldMap[A] _),
        "add to empty is singleton" -> forAll(laws.addEmptyIsSingleton[A] _),
        "add is consisten with foldMap" -> forAll(laws.addMonoidConsistency[A, B] _),
        "contains matches toSortedList" -> forAll(laws.containsMatchesToList[A] _),
        "empty is Monoid.empty" -> (laws.emptyIsEmpty[A]: Prop),
        "add increases size" -> forAll(laws.addIncreasesSize[A] _),
        "minimumOption is the min" -> forAll(laws.minimumOptionIsTheMinimum[A] _),
        "remove decreases size" -> forAll(laws.removeDecreasesSize[A] _),
        "remove min never decreases min" -> forAll(laws.removeMinNeverDecreasesMin[A] _),
        "addAll + toSortedList same as sorting" -> forAll(laws.addAllListSameAsSort[A] _),
        "addAll matches default impl" -> forAll(laws.addAllMatchesProxy[A] _),
        "build matches default impl" -> forAll(laws.buildMatchesProxy[A] _),
        "sortedFoldLeft matches default impl" -> forAll(laws.sortedFoldLeftMatchesProxy[A, B] _),
        "takeLargest matches default impl" -> forAll(laws.takeLargestMatchesProxy[A] _),
        "addIfLarger matches default impl" -> forAll(laws.addIfLargerMatchesProxy[A] _),
        "toSortedList matches default impl" -> forAll(laws.toSortedListMatchesProxy[A] _),
        "addAllLargest matches default impl" -> forAll(laws.addAllLargestMatchesProxy[A] _),
        "unadd matches default impl" -> unaddMatchesProxyProps,
        "pop matches default impl" -> forAll(laws.popMatchesProxy[A] _)
      ) ++ OrderTests[F[A]].order.props
    }
  }
}

object PartiallyOrderedSetTests {
  def apply[F[_]: PartiallyOrderedSet]: PartiallyOrderedSetTests[F] =
    new PartiallyOrderedSetTests[F] {
      def laws = new PartiallyOrderedSetLaws[F] {
        def F = PartiallyOrderedSet[F]
      }
    }
}
