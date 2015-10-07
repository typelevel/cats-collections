package dogs

import cats._
import algebra.Order, algebra.Order._
import scala.{Boolean,Nothing}

abstract class RedBlackTree[A] {
  import RedBlackTree._

  def cata[B](nil: Eval[B], node: (A, Eval[B], Eval[B]) => Eval[B]): Eval[B] = this match {
    case RedBlackNil() => nil
    case RedBlackNode(value, _, left, right) =>
      Eval.defer(node(value, left.cata(nil, node), left.cata(nil, node)))
  }


  def member(a: A)(implicit order: Order[A]): Boolean = this match {
    case RedBlackNil() => false
    case RedBlackNode(value, _, left, right) => compare(a, value) match {
      case 0 => true
      case x if x < 0 => left.member(a)
      case x => right.member(a)
    }
  }

  def insert(newA: A)(implicit order: Order[A]): RedBlackTree[A] = this match {
    case RedBlackNil() => RedBlackNode(newA, Red, RedBlackNil(), RedBlackNil())
    case RedBlackNode(a, color, left, right) => compare(newA, a) match {
      case 0 => this
      case x if x < 0 => balancel(a, color, left.insert(newA), right)
      case x => balancer(a, color, left, right.insert(newA))
    }

  }

  def toLst(implicit order: Order[A]): Lst[A] = this match {
    case RedBlackNil() => El()
    case RedBlackNode(a, _, left, right) => left.toLst ::: (a :: right.toLst)
  }

  private[dogs] def color: Color
}



object RedBlackTree {
  def empty[A]: RedBlackTree[A] = RedBlackNil()

  def _balancedl[A](a: A,
                   ll: RedBlackTree[A], lr: RedBlackTree[A], la: A,
                   right: RedBlackTree[A]) =
    RedBlackNode(a, Red, 
                 RedBlackNode(la, Black, ll, lr),
                 right)

  def _balancedb[A](a: A,
                   ll: RedBlackTree[A], lr: RedBlackTree[A], la: A,
                   rl: RedBlackTree[A], rr: RedBlackTree[A], ra: A) = 
    RedBlackNode(a, Red, 
                 RedBlackNode(la, Black, ll, lr),
                 RedBlackNode(ra, Black, rl, rr))

  def _balancedr[A](a: A,
                   left: RedBlackTree[A],
                   rl: RedBlackTree[A], rr: RedBlackTree[A], ra: A) = 
    RedBlackNode(a, Red, 
                 left,
                 RedBlackNode(ra, Black, rl, rr))

  def balancel[A](a: A, color: Color, left: RedBlackTree[A], right: RedBlackTree[A])(implicit order: Order[A]): RedBlackTree[A] = 
    if(color == Black) {
      left match {
        case RedBlackNode(la, Red, RedBlackNode(lla, Red, lll, llr), lr) =>
          _balancedl(a, lll, llr, lla, right)
        case RedBlackNode(la, Red, ll, RedBlackNode(lra, Red, lrl, lrr)) =>
          _balancedb(lra, ll, lrl, la, lrr, right, a)
        case _ => RedBlackNode(a, color, left, right)
      }
    } else RedBlackNode(a, color, left, right)

  def balancer[A](a: A, color: Color, left: RedBlackTree[A], right: RedBlackTree[A])(implicit order: Order[A]): RedBlackTree[A] = 
    if(color == Black) {
      right match {
        case RedBlackNode(la, Red, rl, RedBlackNode(rra, Red, rrl, rrr)) =>
          _balancedr(a, left, rrl, rrr, rra)
        case RedBlackNode(ra, Red, RedBlackNode(rla, Red, rll, rlr), rr) =>
          _balancedb(rla, left, rll, a, rlr, rr, ra)
        case _ => RedBlackNode(a, color, left, right)
      }
    } else RedBlackNode(a, color, left, right)


  sealed trait Color
  case object Red extends Color
  case object Black extends Color
  case object Empty extends Color

}

case class RedBlackNode[A](value: A, color: RedBlackTree.Color, left: RedBlackTree[A], right: RedBlackTree[A]) extends RedBlackTree[A]

case object RedBlackNil extends RedBlackTree[Nothing] {
  val color = RedBlackTree.Empty

  def apply[A](): RedBlackTree[A] = this.asInstanceOf[RedBlackTree[A]]
  def unapply[A](a: RedBlackTree[A]): Boolean = a.color == RedBlackTree.Empty
}

