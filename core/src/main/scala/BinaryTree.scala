/**
 * Created by Nicolas A Perez (@anicolaspp) on 1/29/16.
 */

package dogs

import Predef._
import dogs.Order.{GT, EQ, LT, Ordering}
import scala.annotation.tailrec
import scala.math

sealed abstract class BinaryTree[A] {
  import BinaryTree._

  val size: Int
  val height: Int

  def isEmpty: Boolean

  def toList(): List[A] = this match {
    case BTNil() =>  El[A]
    case Branch(a, l, r) => l.toList ::: (a :: r.toList)
  }

  def inorder() = toList()

  def min: Option[A] = {
    @tailrec def loop(sub: BinaryTree[A], x: A): A = sub match {
      case BTNil() =>  x
      case Branch(a, l, _) => loop(l, a)
    }

    this match {
      case BTNil() => None()
      case Branch(a, l, _) => Some(loop(l, a))
    }
  }

  def max: Option[A] = {
    @tailrec def loop(sub: BinaryTree[A], x: A): A = sub match {
      case BTNil() =>  x
      case Branch(a, _, r) => loop(r, a)
    }

    this match {
      case BTNil() => None()
      case Branch(a, _, r) => Some(loop(r, a))
    }
  }

  def contains(x: A)(implicit order: Order[A]): Boolean = this match {
    case BTNil() => false

    case Branch(a, l, r) => order.compare(x, a) match {
      case LT => l.contains(x)
      case EQ => true
      case GT => r.contains(x)
    }
  }

  def add(x: A)(implicit order: Order[A]): Branch[A] =
    (this match {
      case BTNil() =>  Branch(x, BinaryTree.empty, BinaryTree.empty)
      case branch @ Branch(a, l, r) =>  order.compare(x, a) match {
        case LT => Branch(a, l.add(x), r)
        case EQ => branch
        case GT => Branch(a, l, r.add(x))
      }
    }).balance

  def +(x: A)(implicit order: Order[A]): BinaryTree[A] = add(x)

  def remove(x: A)(implicit order: Order[A]): BinaryTree[A] =
    this match {
      case BTNil() => BinaryTree.empty
      case Branch(a, l, r) =>
        order.compare(x, a) match {
          case LT => Branch(a, l.remove(x), r).balance
          case GT => Branch(a, l, r.remove(x)).balance
          case EQ => r.min match {
            case None() => l
            case Some(v) => Branch(v,l,r.remove(v)).balance
          }
        }
    }

  def join(another: BinaryTree[A])(implicit order: Order[A]) = {
    // todo, no need to go to list, we need a TreeLoc
    @tailrec def build(sub: BinaryTree[A], xs: List[A]): BinaryTree[A] = xs match {
      case El() =>  sub
      case Nel(h, t) =>  build(sub + h, t)
    }

    another match {
      case BTNil() => this
      case _ => build(this, another.toList())
    }
  }

  def ++(another: BinaryTree[A])(implicit order: Order[A]) = join(another)
}

object BinaryTree {

  def apply[A](a: A): BinaryTree[A] = Branch(a,empty,empty)
  def empty[A]: BinaryTree[A] = BTNil()

  private[dogs] case class Branch[A] private[dogs](value: A,
                                                   left: BinaryTree[A],
                                                   right: BinaryTree[A]) extends BinaryTree[A] {
    val size = left.size + right.size + 1
    val height = java.lang.Math.max(left.height, right.height) + 1

    override def isEmpty: Boolean = false

    private[dogs] def balance: Branch[A] = {
      def rotation(l: Int, r: Int, allow: Int): Ordering =
        if(l - r > allow ) GT
        else if(r - l > allow) LT
        else EQ

      rotation(left.height, right.height, 1) match {
        case EQ => this
        case GT => left match {
          case BTNil() => this
          case Branch(lv,ll,lr) => rotation(ll.height, lr.height, 0) match {
            case LT =>
              val Branch(lrv,lrl,lrr) = lr
              Branch(lrv,Branch(lv, ll, lrl), Branch(value, lrr, right))
            case _ => Branch(lv, ll, Branch(value, lr, right))
          }
        }
        case LT => right match {
          case BTNil() => this
          case Branch(rv,rl,rr) => rotation(rl.height, rr.height, 0) match {
            case GT =>
              val Branch(rlv,rll,rlr) = rl
              Branch(rlv, Branch(value, left, rll), Branch(rv, rlr, rr))
            case _ => Branch(rv, Branch(value, left, rl), rr)
          }
        }
      }
    }
  }

  private[dogs] case object BTNil extends BinaryTree[Nothing] {
    override def isEmpty: Boolean = true

    def apply[A](): BinaryTree[A] = this.asInstanceOf[BinaryTree[A]]

    def unapply[A](a: BinaryTree[A]): Boolean = a.isEmpty

    override val size: Int = 0
    override val height: Int = 0
  }

}

