/**
 * Created by Nicolas A Perez (@anicolaspp) on 1/29/16.
 */

package dogs

import Predef._

import dogs.Order.{GT, EQ, LT}


abstract class BinaryTree[A] {

  def value: Option[A]

  val left: BinaryTree[A]
  val right: BinaryTree[A]


  def isEmpty: Boolean

  def add(x: A)(implicit order: Order[A]): Branch[A] = this match {
    case BTNil()                  =>  Branch(Some(x), BinaryTree.empty, BinaryTree.empty)
    case Branch(Some(a), l, r)    =>  order.compare(x, a) match {
      case LT =>  Branch(Some(a), l.add(x), r)
      case EQ =>  this.asInstanceOf[Branch[A]]
      case GT =>  Branch(Some(a), l, r.add(x))
    }
  }
}

object BinaryTree {
  def apply[A](a: A): BinaryTree[A] = Branch(Some(a),empty,empty)

  def empty[A]: BinaryTree[A] = BTNil()
}

case class Branch[A](value: Option[A],
                      left: BinaryTree[A],
                      right: BinaryTree[A]) extends BinaryTree[A] {

  override def isEmpty: Boolean = false

  def apply(): BinaryTree[A] = this.asInstanceOf[BinaryTree[A]]

  def apply[B](a: B) = Branch[B](Some(a), BTNil(), BTNil())
}

case object BTNil extends BinaryTree[Nothing] {
  override def isEmpty: Boolean = true

  def apply[A](): BinaryTree[A] = this.asInstanceOf[BinaryTree[A]]

  def unapply[A](a: BinaryTree[A]): Boolean = a.isEmpty

  override def value: Option[Nothing] = None()

  override val left: BinaryTree[Nothing] = this
  override val right: BinaryTree[Nothing] = this
}

