package dogs

import dogs.Predef._

/**
 * [[Const]] is a phantom type, it does not contain a value of its second type parameter `B`
 * [[Const]] can be seen as a type level version of `Function.const[A, B]: A => B => A`
 */
final case class Const[A, B](getConst: A) {
  /**
   * changes the type of the second type parameter
   */
  def retag[C]: Const[A, C] =
    this.asInstanceOf[Const[A, C]]

  def ===(that: Const[A, B])(implicit A: Eq[A]): Boolean =
    A.eqv(getConst, that.getConst)

  def compare(that: Const[A, B])(implicit cmp: Order[A]): Order.Ordering =
    cmp(getConst, that.getConst)
}

object Const extends ConstInstances

trait ConstInstances {
  implicit def constEq[A: Eq, B]: Eq[Const[A,B]] = new Eq[Const[A,B]] {
    def eqv(l: Const[A,B], r: Const[A,B]): Boolean = (l === r)
  }

  implicit def constOrder[A: Order, B]: Order[Const[A,B]] = new Order[Const[A,B]] {
    def apply(l: Const[A,B], r: Const[A,B]) = l compare r
  }
}
