package dogs

trait Order[A] {
  def apply(l: A, r: A): Order.Ordering
  def compare(l: A, r: A): Order.Ordering = apply(l,r)
}

object Order {
  sealed trait Ordering
  case object LT extends Ordering
  case object EQ extends Ordering
  case object GT extends Ordering
}
