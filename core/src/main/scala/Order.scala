package dogs

import Predef._

trait Order[A] { self =>
  import Order._
  /**
   * Compare `l` and `r` values returning:
   * Order.LT when `l` is less than `r`
   * Order.EQ when `l` is equal to than `r`
   * Order.GT when `l` is greater than `r`
   */
  def apply(l: A, r: A): Order.Ordering

  /**
   * Compare `l` and `r` values returning:
   * Order.LT when `l` is less than `r`
   * Order.EQ when `l` is equal to than `r`
   * Order.GT when `l` is greater than `r`
   */
  def compare(l: A, r: A): Order.Ordering = apply(l,r)

  /**
   * Given a function that can produce an A from a B, return an
   * Order[B] which uses this ordering after converting Bs to As
   */
  def contramap[B](f: B => A): Order[B] = new Order[B] {
    override def apply(l: B, r: B) = self(f(l), f(r))
  }

  /**
   * Test if the two given values are equal
   */
  def eq(l: A, r: A): Boolean = apply(l, r) == EQ

  /**
   * Test if the left is greater than the right
   */
  def gt(l: A, r: A): Boolean = apply(l, r) == GT

  /**
   * Test if the left is greater than or equal to the right
   */
  def ge(l: A, r: A): Boolean = apply(l, r) != LT

  /**
   * Test if the left is less than the right
   */
  def lt(l: A, r: A): Boolean = apply(l, r) == LT

  /**
   * Test if the left is less than or equal to the right
   */
  def le(l: A, r: A): Boolean = apply(l, r) != GT

  /**
   * Returns the lesser of the two given values.
   */
  def min(l: A, r: A): A = compare(l,r) match {
    case LT => l
    case _ => r
  }

  /**
   * Returns the greater of the two given values.
   */
  def max(l: A, r: A): A = compare(l,r) match {
    case LT => r
    case _ => l
  }
}

object Order {
  sealed trait Ordering
  case object LT extends Ordering
  case object EQ extends Ordering
  case object GT extends Ordering

  /**
   * summon an Order from implicit scope
   */
  def apply[A](implicit A: Order[A]): Order[A] = A

  /**
   * A convenience method for defining an ordering
   */
  def instance[A](f: (A,A) => Ordering): Order[A] = new Order[A] {
    def apply(l: A, r: A): Ordering = f(l,r)
  }
}
