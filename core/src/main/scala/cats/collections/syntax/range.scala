package cats.collections
package syntax

trait RangeSyntax {
  implicit def rangeSyntax[A](a: A): RangeOps[A] = new RangeOps(a)
}

final class RangeOps[A](val from: A) extends AnyVal {
  def toIncl(to: A): Range[A] = Range(from, to)
  def toExcl(to: A)(implicit A: Enum[A]): Range[A] = Range(from, A.pred(to))
}
