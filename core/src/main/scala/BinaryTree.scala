/**
 * Created by Nicolas A Perez (@anicolaspp) on 1/29/16.
 */

package dogs

import Predef._

import dogs.Order.{GT, EQ, LT}

import scala.annotation.tailrec
import scala.List._


abstract class BinaryTree[A] {

  def value: Option[A]

  val left: BinaryTree[A]
  val right: BinaryTree[A]


  def isEmpty: Boolean

  def toLst(): List[A] = this match {
    case BTNil()                =>  El[A]
    case Branch(Some(a), l, r)  =>  l.toLst ::: (a :: r.toLst)
  }

  def inorder() = toLst()

  def preorder(): List[A] = this match {
    case BTNil()                =>  El[A]
    case Branch(Some(a), l, r)  =>  a :: l.preorder ::: r.preorder
  }

  def posorder(): List[A] = this match {
    case BTNil()                =>  El[A]
    case Branch(Some(a), l, r)  =>  l.posorder() ::: r.posorder() ::: Nel(a, El[A])
  }

  def min(): Option[A] =  {
    @tailrec def loop(sub: BinaryTree[A], x: Option[A]): Option[A] = sub match {
      case BTNil()              =>  x
      case Branch(a, l, _)      =>  loop(l, a)
    }

    this match {
      case BTNil()              => None()
      case Branch(a, l, _)      =>  loop(l, a)
    }
  }

  def max(): Option[A] = {
    @tailrec def loop(sub: BinaryTree[A], x: Option[A]): Option[A] = sub match {
      case BTNil()              =>  x
      case Branch(a, _, r)      =>  loop(r, a)
    }

    this match {
      case BTNil()              =>  None()
      case Branch(a, _, r)      =>  loop(r, a)
    }
  }

  def add(x: A)(implicit order: Order[A]): Branch[A] = this match {
    case BTNil()                =>  Branch(Some(x), BinaryTree.empty, BinaryTree.empty)
    case Branch(Some(a), l, r)  =>  order.compare(x, a) match {
      case LT                     =>  Branch(Some(a), l.add(x), r)
      case EQ                     =>  this.asInstanceOf[Branch[A]]
      case GT                     =>  Branch(Some(a), l, r.add(x))
    }
  }

  def +(x: A)(implicit order: Order[A]): Branch[A] = add(x)

  def remove(x: A)(implicit order: Order[A]): BinaryTree[A] = this match {
    case BTNil()                =>  BinaryTree.empty
    case Branch(Some(a), l, r)  =>  order.compare(x, a) match {
      case LT =>  Branch(Some(a), l.remove(x), r)
      case GT =>  Branch(Some(a), l, r.remove(x))
      case EQ => (l, r) match {
        case (BTNil(), BTNil())   => BinaryTree.empty
        case (BTNil(), x)         => x
        case (x, BTNil())         => x
        case (x, y)               => {
          val min = y.min

          min match {
            case Some(a)            =>  Branch(min, x, y.remove(a))
            case None()             =>  x //<- this should never happen
          }
        }
      }
    }
  }


  def join(another: BinaryTree[A])(implicit order: Order[A]) = {

    @tailrec def build(sub: BinaryTree[A], xs: List[A]): BinaryTree[A] = xs match {
      case El()                 =>  sub
      case Nel(h, t)            =>  build(sub + h, t)
    }

    another match {
      case BTNil()              =>  this
      case _                    =>  build(this, another.toLst())
    }
  }

  def ++(another: BinaryTree[A])(implicit order: Order[A]) = join(another)
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

