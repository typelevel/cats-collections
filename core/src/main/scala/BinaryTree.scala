/**
 * Created by Nicolas A Perez (@anicolaspp) on 1/29/16.
 */

package dogs

import Predef._
import dogs.Order.{GT, EQ, LT}
import scala.annotation.tailrec
import scala.math



abstract class BinaryTree[A] {

  def value: Option[A]

  val left: BinaryTree[A]
  val right: BinaryTree[A]

  private [dogs] val size: Int
  private[dogs] val height: Int
  private [dogs] def balanceFactor: Int
  private[dogs] def update(): BinaryTree[A]

  def isEmpty: Boolean

  def toLst(): List[A] = this match {
    case BTNil()                      =>  El[A]
    case Branch(Some(a), l, r, _, _)  =>  l.toLst ::: (a :: r.toLst)
  }

  def inorder() = toLst()

  def preorder(): List[A] = this match {
    case BTNil()                      =>  El[A]
    case Branch(Some(a), l, r, _, _)  =>  a :: l.preorder ::: r.preorder
  }

  def posorder(): List[A] = this match {
    case BTNil()                      =>  El[A]
    case Branch(Some(a), l, r, _, _)  =>  l.posorder() ::: r.posorder() ::: Nel(a, El[A])
  }

  def min(): Option[A] =  {
    @tailrec def loop(sub: BinaryTree[A], x: Option[A]): Option[A] = sub match {
      case BTNil()              =>  x
      case Branch(a, l, _, _, _)      =>  loop(l, a)
    }

    this match {
      case BTNil()                    => None()
      case Branch(a, l, _, _, _)      =>  loop(l, a)
    }
  }

  def max(): Option[A] = {
    @tailrec def loop(sub: BinaryTree[A], x: Option[A]): Option[A] = sub match {
      case BTNil()                    =>  x
      case Branch(a, _, r, _, _)      =>  loop(r, a)
    }

    this match {
      case BTNil()                    =>  None()
      case Branch(a, _, r, _, _)      =>  loop(r, a)
    }
  }

  def contains(x: A)(implicit order: Order[A]): Boolean = this match {
    case BTNil()                      =>  false

    case Branch(Some(a), l, r, _, _)  => order.compare(x, a) match {
      case LT                             =>  l.contains(x)
      case EQ                             =>  true
      case GT                             =>  r.contains(x)
    }
  }

  def add(x: A)(implicit order: Order[A]): Branch[A] = {
    def insert(x: A, order: Order[A])  = this match {
      case BTNil()                    =>  Branch(Some(x), BinaryTree.empty, BinaryTree.empty)
      case Branch(Some(a), l, r, _, _)=>  order.compare(x, a) match {
        case LT                     =>  Branch(Some(a), l.add(x)(order), r)
        case EQ                     =>  this.asInstanceOf[Branch[A]]
        case GT                     =>  Branch(Some(a), l, r.add(x)(order))
      }
    }

    balance(insert(x, order)).asInstanceOf[Branch[A]]
  }

  def +(x: A)(implicit order: Order[A]): BinaryTree[A] = add(x)

  def remove(x: A)(implicit order: Order[A]): BinaryTree[A] = {
    def del(x:A)(implicit order: Order[A]): BinaryTree[A] = this match {
      case BTNil() => BinaryTree.empty
      case Branch(Some(a), l, r, _, _) => order.compare(x, a) match {
        case LT => Branch(Some(a), l.remove(x), r)
        case GT => Branch(Some(a), l, r.remove(x))
        case EQ => (l, r) match {
          case (BTNil(), BTNil()) => BinaryTree.empty
          case (BTNil(), x) => x
          case (x, BTNil()) => x
          case (x, y) => {
            val min = y.min

            min match {
              case Some(a) => Branch(min, x, y.remove(a))
              case None() => x //<- this should never happen
            }
          }
        }
      }
    }

    balance(del(x))
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

  private def balance(tree: BinaryTree[A]): BinaryTree[A] = {
    var t = tree.update

    if (t.balanceFactor > 1) {
      if (t.right.balanceFactor <= 0) {
        t = Branch(t.value, t.left, rotate(t.right, 1), t.size, t.height)
      }
      t = rotate(t, -1)
    } else {
      if (t.balanceFactor < -1)
      {
        if (t.left.balanceFactor >= 0){
          t = Branch(t.value, rotate(t.left, -1), t.right, t.size, t.height)
        }
        t = rotate(t, 1)
      }
    }

    return t
  }

  private def rotate(x: BinaryTree[A], direction: Int): BinaryTree[A] = {
    if (direction < 0) {  // left

      if (x == BTNil() || x.right == BTNil()) return x

      val y = x.right
      val a = Branch(x.value, x.left, BTNil(), x.size, x.height).update()

      val b = Branch(y.value, a, y.right, y.size, y.height).update()

      return b
    }
    else { //right
      if (x == BTNil() || x.left == BTNil()) return x

      val y = x.left
      val a = Branch(x.value, BTNil(), x.right, x.size, x.height).update()

      val b = Branch(y.value, y.left, a, y.size, y.height).update()

      return b
    }
  }

}



case class Branch[A](value: Option[A],
                     left: BinaryTree[A],
                     right: BinaryTree[A],
                     private [dogs] val size: Int = 0,
                     private[dogs] val height: Int = 0) extends BinaryTree[A] {

  private [dogs] override def balanceFactor: Int = this match {
    case Branch(_, BTNil(), BTNil(), _, _)    =>  0
    case Branch(_, BTNil(), r, _, _)          =>  r.asInstanceOf[Branch[A]].height
    case Branch(_, l, BTNil(), _, _)          =>  -1 * l.asInstanceOf[Branch[A]].height
    case Branch(_, l, r, _, _)                =>  r.asInstanceOf[Branch[A]].height - l.asInstanceOf[Branch[A]].height
  }

  private [dogs] override def update(): BinaryTree[A] = this match {
    case Branch(a, BTNil(), BTNil(), _, _)  =>  Branch(a, BTNil(), BTNil(), 1, 1)
    case Branch(a, BTNil(), r, _, _)        =>  {
      val s = 1 + r.asInstanceOf[Branch[A]].size
      val h = 1 + r.asInstanceOf[Branch[A]].height

      Branch(a, BTNil(), r, s, h)
    }
    case Branch(a, l, BTNil(), _, _)        =>  {
      val s = 1 + l.size
      val h = 1 + l.height

      Branch(a, l, BTNil(), s, h)
    }
    case Branch(a, l, r, _, _)              =>  {
      val s = l.size + r.size
      val h = 1 + math.max(l.height, r.height)

      Branch(a, l, r, s, h)
    }
  }


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

  private [dogs] override def update(): BinaryTree[Nothing] = this
  private [dogs] override def balanceFactor: Int = 0
  private [dogs] override val size: Int = 0
  private [dogs] override val height: Int = 0
}

object BinaryTree {
  def apply[A](a: A): BinaryTree[A] = Branch(Some(a),empty,empty)

  def empty[A]: BinaryTree[A] = BTNil()
}

