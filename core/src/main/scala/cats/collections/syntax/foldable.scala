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

package cats.collections
package syntax

import cats.{Foldable, Order, Semigroup}

trait FoldableSyntax {
  implicit def foldableSyntax[F[_]: Foldable, A](fa: F[A]): FoldableOps[F, A] =
    new FoldableOps(fa)
}

final class FoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]) {
  def toCatsVector: Vector[A] =
    F.foldLeft[A, Vector[A]](fa, Vector.empty)(_ :+ _)

  def toCatsMap[K, V](implicit K: Order[K], ev: A =:= (K, V)): AvlMap[K, V] = {
    F.foldLeft(fa, AvlMap.empty[K, V])(_ + _)
  }

  def toCatsMultiMap[K, V](implicit K: Order[K], ev: A =:= (K, V), V: Semigroup[V]): AvlMap[K, V] = {
    F.foldLeft(fa, AvlMap.empty[K, V]) { (m, a) =>
      val (k, v) = ev(a)
      m.updateAppend(k, v)
    }
  }
}
