package dogs

import Predef._
import dogs.Order.{GT, EQ, LT, Ordering}
import dogs.std.intOrder
import scala.annotation.tailrec
import scala.math


/**
 * An immutable, ordered Set
 * 
 * This datastructure maintains balance using the
 * [AVL](https://en.wikipedia.org/wiki/AVL_tree) algorithm.
 */
sealed abstract class Set[A] {
  import Set._

  /**
   * The number of items in the Set.
   * O(1)
   */
  val size: Int

  /**
   * Returns `true` if the Set is the empty Set.
   * O(1)
   */
  def isEmpty: Boolean

  /**
   * Map a function on all values of the set
   */
  def map[B: Order](f: A => B): Set[B] =
    foldLeft[Set[B]](empty)((s,a) => s + f(a))

  /**
   * Map a function on all values of the set
   */
  def flatMap[B: Order](f: A => Set[B]): Set[B] =
    foldLeft[Set[B]](empty)((s,a) => s ++ f(a))

  /**
   * Return the sorted list of elements.
   * O(n)
   */
  def toList(): List[A] = this match {
    case BTNil() =>  El[A]
    case Branch(a, l, r) => l.toList ::: (a :: r.toList)
  }

  /**
   * Retruns None if the Set is empty, otherwise returns the minumum
   * element.
   * O(log n)
   */
  def min: Option[A] = {
    @tailrec def loop(sub: Set[A], x: A): A = sub match {
      case BTNil() =>  x
      case Branch(a, l, _) => loop(l, a)
    }

    this match {
      case BTNil() => None()
      case Branch(a, l, _) => Some(loop(l, a))
    }
  }

  /**
   * Retruns `None` if the Set is empty, otherwise returns the maximum
   * element.
   * O(log n)
   */
  def max: Option[A] = {
    @tailrec def loop(sub: Set[A], x: A): A = sub match {
      case BTNil() =>  x
      case Branch(a, _, r) => loop(r, a)
    }

    this match {
      case BTNil() => None()
      case Branch(a, _, r) => Some(loop(r, a))
    }
  }

  /**
   * fold the elements together from min to max, using the passed
   * seed, and accumulator function.
   * O(n)
   */
  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case BTNil() => z
    case Branch(v, l, r) => r.foldLeft(f(l.foldLeft(z)(f), v))(f)
  }

  /**
   * Find the minimum element matching the given predicate. Returns
   * None if there is no element matching the predicte.
   * O(log n)
   */
  def find(pred: A => Boolean): Option[A] = this match {
    case BTNil() => None()
    case Branch(v, l, r) =>
      l.find(pred) orElse (if(pred(v)) Some(v) else r.find(pred))
  }

  /**
   * Returns `true` if the given element is in the Set.
   * O(log n)
   */
  def contains(x: A)(implicit order: Order[A]): Boolean = this match {
    case BTNil() => false

    case Branch(a, l, r) => order.compare(x, a) match {
      case LT => l.contains(x)
      case EQ => true
      case GT => r.contains(x)
    }
  }

  /**
   * Add's the given element to the Set if it is not already present.
   * O(log n)
   */
  def add(x: A)(implicit order: Order[A]): Branch[A] =
    (this match {
      case BTNil() =>  Branch(x, Set.empty, Set.empty)
      case branch @ Branch(a, l, r) =>  order.compare(x, a) match {
        case LT => Branch(a, l.add(x), r)
        case EQ => Branch(x, l, r)
        case GT => Branch(a, l, r.add(x))
      }
    }).balance


  /**
   * Add's the given element to the Set if it is not already present.
   * O(log n)
   */
  def +(x: A)(implicit order: Order[A]): Set[A] = add(x)

  /**
   * Return a Set which does not contain the given element.
   * O(log n)
   */
  def remove(x: A)(implicit order: Order[A]): Set[A] =
    this match {
      case BTNil() => Set.empty
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

  // STU: this is used by Map, not sure what to do about this
  private[dogs] def removef[B](x: B, f: A => B)(implicit B: Order[B]): Set[A] =
    this match {
      case BTNil() => Set.empty
      case Branch(a, l, r) =>
        B.compare(x, f(a)) match {
          case LT => Branch(a, l.removef(x, f), r).balance
          case GT => Branch(a, l, r.removef(x, f)).balance
          case EQ => r.min match {
            case None() => l
            case Some(v) =>
              Branch(v,l,r.removef(f(v), f)).balance
          }
        }
    }

  /**
   * Return a Set containing the union of elements with this Set and
   * the given Set.
   * O(n log n)
   */
  def union(another: Set[A])(implicit order: Order[A]) = another.foldLeft(this)(_ + _)

  /**
   * Return a Set containing the union of elements with this Set and
   * the given Set.
   * O(n log n)
   */
  def |(another: Set[A])(implicit order: Order[A]) = this union another

  /**
   * Return a Set containing the intersection of elements with this Set and
   * the given Set.
   * O(n log n)
   */
  def intersect(another: Set[A])(implicit order: Order[A]) = {
    def _intersect(small: Set[A], large: Set[A]): Set[A] =
      small.foldLeft[Set[A]](empty)((t,a) => if(large.contains(a)) t + a else t)

    intOrder(this.size, another.size) match {
      case LT => _intersect(this, another)
      case _ => _intersect(another,this)
    }
  }

  /**
   * Return a Set containing the intersection of elements with this Set and
   * the given Set.
   * O(n log n)
   */
  def &(another: Set[A])(implicit order: Order[A]) = this intersect another

  /**
   * Return a Set containing the union of elements with this Set and
   * the given Set.
   * O(n log n)
   */
  def ++(another: Set[A])(implicit order: Order[A]) = this union another

  /**
   * Return a Set that has any elements appearing in the removals set removed
   * O(n log n)
   */
  def diff(removals: Set[A])(implicit order: Order[A]) =
    removals.foldLeft(this)(_ remove _)

  /**
   * Return a scala set containing the elments in the Set
   * O(n)
   */
  def toScalaSet: scala.collection.immutable.Set[A] = {
    import scala.collection.immutable.{Set => SSet}
    foldLeft[SSet[A]](SSet.empty)(_ + _)
  }

  private[dogs] val height: Int
}

object Set {

  /**
   * Create a set with the given elements. 
   */
  def apply[A: Order](as: A*): Set[A] =
    as.foldLeft[Set[A]](empty)(_ + _)

  /**
   * The empty Set.
   */
  def empty[A]: Set[A] = BTNil()

  private[dogs] case class Branch[A](value: A,
                                     left: Set[A],
                                     right: Set[A]) extends Set[A] {

    val size = left.size + right.size + 1
    val height = java.lang.Math.max(left.height, right.height) + 1

    override def isEmpty: Boolean = false

    private[dogs] def balance: Branch[A] = {

      // Determine the direction that the tree should be rotated,
      // given the allowed amount of imbalance.
      // Returns LT when a left rotation is called for.
      // Returns GT when a right rotation is called for.
      // Returns EQ when the tree is withing the allowance.
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

  private[dogs] case object BTNil extends Set[Nothing] {
    override def isEmpty: Boolean = true

    def apply[A](): Set[A] = this.asInstanceOf[Set[A]]

    def unapply[A](a: Set[A]): Boolean = a.isEmpty

    override val size: Int = 0
    override val height: Int = 0
  }

}

