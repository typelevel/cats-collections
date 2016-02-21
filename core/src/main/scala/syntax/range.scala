package dogs
package syntax

import Predef._

trait RangeSyntax {
  implicit def rangeSyntax[A](a: A): RangeOps[A] = new RangeOps(a)
}

final class RangeOps[A](val from: A) extends AnyVal {
  def to(to: A)(implicit A: Enum[A]): Range[A] = Range(from, to)
  def until(to: A)(implicit A: Enum[A]): Range[A] = Range(from, A.pred(to))
}
