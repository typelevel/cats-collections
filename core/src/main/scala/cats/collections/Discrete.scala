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

/**
 * Represent discrete operations that can be performed on A
 */
trait Discrete[@specialized(Specializable.Integral) A] extends Serializable {

  /**
   * Return the successor of x.
   */
  def succ(x: A): A

  /**
   * Returns the predecessor of x.
   */
  def pred(x: A): A

  /**
   * Returns true if x and y are consecutive.
   */
  def adj(x: A, y: A): Boolean = succ(x) == y

  /**
   * Returns the inverse of this discrete (succ and pred are flipped).
   */
  def inverse: Discrete[A] =
    Discrete.inverse(this)
}

object Discrete {

  /**
   * Summons an implicit instance of Discrete[A] from the implicit scope.
   */
  @inline final def apply[A](implicit instance: Discrete[A]): Discrete[A] =
    instance

  /**
   * Returns the inverse of the provided discrete (succ and pred are flipped).
   */
  final def inverse[A](discrete: Discrete[A]): Discrete[A] =
    new Discrete[A] {
      override def succ(x: A): A = discrete.pred(x)
      override def pred(x: A): A = discrete.succ(x)
    }

  implicit def integralDiscrete[@specialized(Specializable.Integral) I](implicit I: Integral[I]): Discrete[I] =
    new Discrete[I] {
      import Integral.Implicits._
      override def succ(x: I): I = x + I.one
      override def pred(x: I): I = x - I.one
    }
}
