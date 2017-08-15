/**
 * Created by nperez on 3/28/16.
 */

package dogs

import cats._
import dogs.Predef._

/**
 * `Heap` is a Purely Functional Binary Heap. Binary Heaps are not common in the functional space, especially because
 * their implementation depends on mutable arrays in order to gain in performance. This functional binary heap is based
 * on Vladimir Kostyukov's paper and it does support the basic operations on a heap without compromising performance.
 *
 * It is important to note that we can, in fact, to create the Binary Heap in order O(n) from a `List` using the
 * function `heapify`.
 */
sealed abstract class Heap[A] {

  import Heap._

  /**
   * Internal representation of the min value to avoid deconstruction of `min: Option[A]` since min is heavily used.
   */
  private [dogs] val min: A

  /**
   * Returns min value on the heap.
   */
  def getMin: Option[A]

  private [dogs] def left: Heap[A]

  private [dogs] def right: Heap[A]

  /**
   * Returns the size of the heap.
   */
  def size: Int

  /**
   * Returns the height of the heap.
   */
  def height: Int

  /**
   * Verifies if the heap is empty.
   */
  def isEmpty: Boolean


  /**
   * Insert a new element into the heap.
   * Order O(log n)
   */
  def add(x: A)(implicit order: Order[A]): Heap[A] =
    if (isEmpty)
      Heap(x, Leaf(), Leaf())
    else if (left.size < scala.math.pow(2, left.height.toDouble) - 1)
      bubbleUp(min, left.add(x), right)
    else if (right.size < scala.math.pow(2, right.height.toDouble) - 1)
      bubbleUp(min, left, right.add(x))
    else if (right.height < left.height)
      bubbleUp(min, left, right.add(x))
    else
      bubbleUp(min, left.add(x), right)

  /**
   * Build a heap using a list.
   * Order O(n)
   */
  def heapify(a: List[A])(implicit order: Order[A]): Heap[A] = {
    def loop(i: Int, xs: scala.List[A]): Heap[A] =
      if (i < xs.length) {
        bubbleDown(xs(i), loop(2 * i + 1, xs), loop(2 * i + 2, xs))
      }
      else {
        Leaf()
      }

    loop(0, a.toScalaList)
  }

  /**
   * Remove the min element from the heap (the root).
   * Order O(log n)
   */
  def remove()(implicit order: Order[A]): Heap[A] = this match {
    case Leaf()                 =>  Leaf()
    case Branch(_, l, r, _, _)  =>  bubbleRootDown(mergeChildren(l, r))
  }

  /**
   * Returns a sorted list of the elements within the heap.
   */
  def toList()(implicit order: Order[A]): List[A] = this match {
    case Leaf()                       =>  El[A]
    case Branch(m, l, r, _, _)        =>  Nel(m, remove.toList)
  }

  /**
   * Alias for add
   */
  def +(x: A)(implicit order: Order[A]): Heap[A] = add(x)

  /**
   * Alias for remove
   */
  def --(implicit order: Order[A]): Heap[A] = remove()

}

object Heap {

  def empty[A]: Heap[A] = Leaf()

  def apply[A](x: A): Heap[A] = Branch(x, empty, empty, 1, 1)

  def apply[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] =
    Branch(x, l, r, l.size + r.size + 1, scala.math.max(l.height, r.height) + 1)

  private [dogs] case class Branch[A](min: A, left: Heap[A], right: Heap[A], size: Int, height: Int) extends Heap[A] {
    override def isEmpty: Boolean = false

    override def getMin: Option[A] = Some(min)
  }

  private [dogs] case object Leaf extends Heap[Option[Nothing]] {
    def apply[A](): Heap[A] = this.asInstanceOf[Heap[A]]

    def unapply[A](heap: Heap[A]): Boolean = heap.isEmpty

    override def size: Int = 0

    override def height: Int = 0

    override def left: Heap[Option[Nothing]] = Leaf

    override def right: Heap[Option[Nothing]] = Leaf

    override def isEmpty: Boolean = true

    override def getMin: Option[Option[Nothing]] = None()

    override private[dogs] val min: Option[Nothing] = None()
  }

  private [dogs] def bubbleUp[A](x: A, l: Heap[A], r: Heap[A])(implicit order: Order[A]): Heap[A] = (l, r) match {
    case (Branch(y, lt, rt, _, _), _) if order.gt(x , y) =>
      Heap(y, Heap(x, lt, rt), r)
    case (_, Branch(z, lt, rt, _, _)) if order.gt(x , z) =>
      Heap(z, l, Heap(x, lt, rt))
    case (_, _) => Heap(x, l, r)
  }

  private [dogs] def bubbleDown[A](x: A, l: Heap[A], r: Heap[A])(implicit order: Order[A]): Heap[A] = (l, r) match {
    case (Branch(y, _, _, _, _), Branch(z, lt, rt, _, _))
      if (order.lt(z , y) && order.gt(x , z))                 => Heap(z, l, bubbleDown(x, lt, rt))
    case (Branch(y, lt, rt, _, _), _)
      if order.gt(x , y)                                      => Heap(y, bubbleDown(x, lt, rt), r)
    case (_, _)                                               => Heap(x, l, r)
  }

  private [dogs] def bubbleRootDown[A](h: Heap[A])(implicit order: Order[A]): Heap[A] =
    if (h.isEmpty) {
      Leaf()
    }
    else {
      bubbleDown(h.min, h.left, h.right)
    }

  private [dogs] def mergeChildren[A](l: Heap[A], r: Heap[A]): Heap[A] =
    if (l.isEmpty && r.isEmpty) {
      Leaf()
    }
    else if (l.size < scala.math.pow(2, l.height.toDouble) - 1) {
      floatLeft(l.min, mergeChildren(l.left, l.right), r)
    }
    else if (r.size < scala.math.pow(2, r.height.toDouble) - 1) {
      floatRight(r.min, l, mergeChildren(r.left, r.right))
    }
    else if (r.height < l.height) {
      floatLeft(l.min, mergeChildren(l.left, l.right), r)
    }
    else {
      floatRight(r.min, l, mergeChildren(r.left, r.right))
    }

  private [dogs] def floatLeft[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] = l match {
    case Branch(y, lt, rt, _, _) => Heap(y, Heap(x, lt, rt), r)
    case _ => Heap(x, l, r)
  }

  private [dogs] def floatRight[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] = r match {
    case Branch(y, lt, rt, _, _) => Heap(y, l, Heap(x, lt, rt))
    case _ => Heap(x, l, r)
  }

  implicit def toShowable[A](implicit s: Show[A], order: Order[A]): Show[Heap[A]] = new Show[Heap[A]] {
    override def show(f: Heap[A]): Predef.String = f.toList() match {
      case El()       => "[]"
      case Nel(h, t)  => t.foldLeft("[" + s.show(h))((acc, r) => acc + ", " + s.show(r)) + "]"
    }
  }
}
