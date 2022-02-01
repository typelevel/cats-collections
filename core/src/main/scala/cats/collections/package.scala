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

package cats

import scala.annotation.tailrec

package object collections {

  @deprecated("ISet has been renamed to Predicate.", "cats-collections 0.8.0")
  type ISet[K] = Predicate[K]
  @deprecated("ISet has been renamed to Predicate.", "cats-collections 0.8.0")
  val ISet = Predicate

  @tailrec private[cats] def iteratorEq[A: Eq](i1: Iterator[A], i2: Iterator[A]): Boolean = {
    val i1N = i1.hasNext
    val i2N = i2.hasNext
    if (!i1N && !i2N)
      true
    else if (i1N != i2N)
      false
    else
      Eq[A].eqv(i1.next(), i2.next()) && iteratorEq(i1, i2)
  }

}
