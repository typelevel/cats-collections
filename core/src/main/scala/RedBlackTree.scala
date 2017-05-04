package dogs

import cats._
import cats.syntax.all._
import algebra.Order, algebra.Order._
import scala.{Boolean,Nothing}
import scala.annotation.tailrec

abstract class RedBlackTree[A] {
  import RedBlackTree._

  def cata[B](nil: Eval[B], node: (A, Eval[B], Eval[B]) => Eval[B]): Eval[B] = this match {
    case RBNil() => nil
    case RBNode(value, _, left, right) =>
      Eval.defer(node(value, left.cata(nil, node), left.cata(nil, node)))
  }

  // preoder / postorder should probably be lazy
  def preorder[B](b: B, f: (B,A) => B): B = {
    def go(b: B, n: RBNode[A]): B = n match {
      case RBNode(a, _, RBNil(), RBNil()) => f(b,a)
      case RBNode(a, _, RBNil(), r : RBNode[A]) => go(f(b,a), r)
      case RBNode(a, _, l : RBNode[A], RBNil()) => f(go(b, l), a)
      case RBNode(a, _, l : RBNode[A], r : RBNode[A]) => go(f(go(b, l), a), r)
    }
    this match {
      case RBNil() => b
      case t: RBNode[A] => go(b,t)
    }
  }

  def postorder[B](b: B, f: (A,B) => B): B = {
    def go(b: B, n: RBNode[A]): B = n match {
      case RBNode(a, _, RBNil(), RBNil()) => f(a,b)
      case RBNode(a, _, l : RBNode[A], RBNil()) => go(f(a,b), l)
      case RBNode(a, _, RBNil(), r : RBNode[A]) => f(a,go(b, r))
      case RBNode(a, _, l : RBNode[A], r : RBNode[A]) => go(f(a, go(b, r)), l)
    }
    this match {
      case RBNil() => b
      case t: RBNode[A] => go(b,t)
    }
  }

  def exists(pred: A => Boolean): Boolean = 
    cata[Boolean](Eval.now(false),
                  {(a,l,r) => if(pred(a)) Eval.now(true) else l.ifM(l,r)}).value


  def member(a: A)(implicit order: Order[A]): Boolean = this match {
    case RBNil() => false
    case RBNode(value, _, left, right) => compare(a, value) match {
      case 0 => true
      case x if x < 0 => left.member(a)
      case x => right.member(a)
    }
  }

  def insert(newA: A)(implicit order: Order[A]): RedBlackTree[A] = this match {
    case RBNil() => RBNode(newA, Red, RBNil(), RBNil())
    case RBNode(a, color, left, right) => compare(newA, a) match {
      case 0 => this
      case x if x < 0 => balancel(a, color, left.insert(newA), right)
      case x => balancer(a, color, left, right.insert(newA))
    }
  }

  def toLst(implicit order: Order[A]): Lst[A] = this match {
    case RBNil() => El()
    case RBNode(a, _, left, right) => left.toLst ::: (a :: right.toLst)
  }

  def union(other: RedBlackTree[A])(implicit order: Order[A]): RedBlackTree[A] = other.preorder[RedBlackTree[A]](this,  _ insert _)



  private[dogs] def color: Color
}



object RedBlackTree {
  def empty[A]: RedBlackTree[A] = RBNil()

  def fromLst[A : Order](lst: Lst[A]) = lst.foldLeft(empty[A])((t, a) => t insert a)

  def _balancedl[A](a: A,
                   ll: RedBlackTree[A], lr: RedBlackTree[A], la: A,
                   right: RedBlackTree[A]) =
    RBNode(a, Red, 
                 RBNode(la, Black, ll, lr),
                 right)

  def _balancedb[A](a: A,
                   ll: RedBlackTree[A], lr: RedBlackTree[A], la: A,
                   rl: RedBlackTree[A], rr: RedBlackTree[A], ra: A) = 
    RBNode(a, Red, 
                 RBNode(la, Black, ll, lr),
                 RBNode(ra, Black, rl, rr))

  def _balancedr[A](a: A,
                   left: RedBlackTree[A],
                   rl: RedBlackTree[A], rr: RedBlackTree[A], ra: A) = 
    RBNode(a, Red, 
                 left,
                 RBNode(ra, Black, rl, rr))

  def balancel[A](a: A, color: Color, left: RedBlackTree[A], right: RedBlackTree[A])(implicit order: Order[A]): RedBlackTree[A] = 
    if(color == Black) {
      left match {
        case RBNode(la, Red, RBNode(lla, Red, lll, llr), lr) =>
          _balancedl(a, lll, llr, lla, right)
        case RBNode(la, Red, ll, RBNode(lra, Red, lrl, lrr)) =>
          _balancedb(lra, ll, lrl, la, lrr, right, a)
        case _ => RBNode(a, color, left, right)
      }
    } else RBNode(a, color, left, right)

  def balancer[A](a: A, color: Color, left: RedBlackTree[A], right: RedBlackTree[A])(implicit order: Order[A]): RedBlackTree[A] = 
    if(color == Black) {
      right match {
        case RBNode(la, Red, rl, RBNode(rra, Red, rrl, rrr)) =>
          _balancedr(a, left, rrl, rrr, rra)
        case RBNode(ra, Red, RBNode(rla, Red, rll, rlr), rr) =>
          _balancedb(rla, left, rll, a, rlr, rr, ra)
        case _ => RBNode(a, color, left, right)
      }
    } else RBNode(a, color, left, right)


  sealed trait Color
  case object Red extends Color
  case object Black extends Color
  case object Empty extends Color

}

case class RBNode[A](value: A, color: RedBlackTree.Color, left: RedBlackTree[A], right: RedBlackTree[A]) extends RedBlackTree[A]

case object RBNil extends RedBlackTree[Nothing] {
  val color = RedBlackTree.Empty

  def apply[A](): RedBlackTree[A] = this.asInstanceOf[RedBlackTree[A]]
  def unapply[A](a: RedBlackTree[A]): Boolean = a.color == RedBlackTree.Empty
}

