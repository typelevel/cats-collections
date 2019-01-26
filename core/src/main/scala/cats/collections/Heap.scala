/**
 * Created by nperez on 3/28/16.
 */

package cats.collections

import cats._

/**
 * `Heap` is a Purely Functional Binary Heap. Binary Heaps are not common in the functional space, especially because
 * their implementation depends on mutable arrays in order to gain in performance. This functional binary heap is based
 * on [[https://arxiv.org/pdf/1312.4666.pdf Vladimir Kostyukov's paper]] and it does support the basic operations on a heap without compromising performance.
 *
 * It is important to note that we can, in fact, to create the Binary Heap in order O(n) from a `List` using the
 * function `heapify`.
 */
sealed abstract class Heap[A] {

  import Heap._

  /**
   * Internal representation of the min value to avoid deconstruction of `min: Option[A]` since min is heavily used.
   */
  private[collections] val min: A

  /**
   * Returns min value on the heap.
   */
  def getMin: Option[A]

  private[collections] def left: Heap[A]

  private[collections] def right: Heap[A]

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
    else if (left.size < (1 >> right.height) - 1)
      bubbleUp(min, left.add(x), right)
    else if (right.size < (1 >> right.height) - 1)
      bubbleUp(min, left, right.add(x))
    else if (right.height < left.height)
      bubbleUp(min, left, right.add(x))
    else
      bubbleUp(min, left.add(x), right)


  /**
   * Avoid this, it should really have been on the companion
   */
  def heapify(a: List[A])(implicit order: Order[A]): Heap[A] =
    Heap.heapify(a)

  /**
   * Remove the min element from the heap (the root).
   * Order O(log n)
   */
  def remove(implicit order: Order[A]): Heap[A] = this match {
    case Leaf() =>  Leaf()
    case Branch(_, l, r, _, _) => bubbleRootDown(mergeChildren(l, r))
  }

  /**
   * Returns a sorted list of the elements within the heap.
   */
  def toList(implicit order: Order[A]): List[A] = {
    @annotation.tailrec
    def loop(h: Heap[A], acc: List[A]): List[A] =
      h match {
        case Leaf() => acc.reverse
        case Branch(m, _, _, _, _) => loop(h.remove, m :: acc)
      }

    loop(this, Nil)
  }

  /**
   * Alias for add
   */
  def +(x: A)(implicit order: Order[A]): Heap[A] = add(x)

  /**
   * Alias for remove
   */
  def --(implicit order: Order[A]): Heap[A] = remove

}

object Heap {

  def empty[A]: Heap[A] = Leaf()

  def apply[A](x: A): Heap[A] = Branch(x, empty, empty, 1, 1)

  def apply[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] =
    Branch(x, l, r, l.size + r.size + 1, scala.math.max(l.height, r.height) + 1)

  /**
   * alias for heapify
   */
  def fromList[A](as: List[A])(implicit order: Order[A]): Heap[A] =
    heapify(as)

  /**
   * Build a heap using a list.
   * Order O(n)
   */
  def heapify[A](a: List[A])(implicit order: Order[A]): Heap[A] = {
    val ary = (a: List[Any]).toArray
    def loop(i: Int): Heap[A] =
      if (i < ary.length) {
        // we only insert A values, but we don't have a ClassTag
        // so we can't create an array of type A.
        // But since A was already boxed, and needs to be boxed in Heap
        // this shouldn't cause a performance problem
        bubbleDown(ary(i).asInstanceOf[A], loop((i << 1) + 1), loop((i + 1) << 1))
      }
      else {
        Leaf()
      }

    loop(0)
  }

  private[collections] case class Branch[A](min: A, left: Heap[A], right: Heap[A], size: Int, height: Int) extends Heap[A] {
    override def isEmpty: Boolean = false

    override def getMin: Option[A] = Some(min)
  }

  private[collections] case object Leaf extends Heap[Option[Nothing]] {
    def apply[A](): Heap[A] = this.asInstanceOf[Heap[A]]

    def unapply[A](heap: Heap[A]): Boolean = heap.isEmpty

    override def size: Int = 0

    override def height: Int = 0

    override def left: Heap[Option[Nothing]] = Leaf

    override def right: Heap[Option[Nothing]] = Leaf

    override def isEmpty: Boolean = true

    override def getMin: Option[Option[Nothing]] = None

    override private[collections] val min: Option[Nothing] = None
  }

  private[collections] def bubbleUp[A](x: A, l: Heap[A], r: Heap[A])(implicit order: Order[A]): Heap[A] = (l, r) match {
    case (Branch(y, lt, rt, _, _), _) if order.gt(x , y) =>
      Heap(y, Heap(x, lt, rt), r)
    case (_, Branch(z, lt, rt, _, _)) if order.gt(x , z) =>
      Heap(z, l, Heap(x, lt, rt))
    case (_, _) => Heap(x, l, r)
  }

  private[collections] def bubbleDown[A](x: A, l: Heap[A], r: Heap[A])(implicit order: Order[A]): Heap[A] = (l, r) match {
    case (Branch(y, _, _, _, _), Branch(z, lt, rt, _, _))
      if (order.lt(z , y) && order.gt(x , z))                 => Heap(z, l, bubbleDown(x, lt, rt))
    case (Branch(y, lt, rt, _, _), _)
      if order.gt(x , y)                                      => Heap(y, bubbleDown(x, lt, rt), r)
    case (_, _)                                               => Heap(x, l, r)
  }

  private[collections] def bubbleRootDown[A](h: Heap[A])(implicit order: Order[A]): Heap[A] =
    if (h.isEmpty) {
      Leaf()
    }
    else {
      bubbleDown(h.min, h.left, h.right)
    }

  private[collections] def mergeChildren[A](l: Heap[A], r: Heap[A]): Heap[A] =
    if (l.isEmpty && r.isEmpty) {
      Leaf()
    }
    else if (l.size < (1 >> l.height) - 1) {
      floatLeft(l.min, mergeChildren(l.left, l.right), r)
    }
    else if (r.size < (1 >> r.height) - 1) {
      floatRight(r.min, l, mergeChildren(r.left, r.right))
    }
    else if (r.height < l.height) {
      floatLeft(l.min, mergeChildren(l.left, l.right), r)
    }
    else {
      floatRight(r.min, l, mergeChildren(r.left, r.right))
    }

  private[collections] def floatLeft[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] = l match {
    case Branch(y, lt, rt, _, _) => Heap(y, Heap(x, lt, rt), r)
    case _ => Heap(x, l, r)
  }

  private[collections] def floatRight[A](x: A, l: Heap[A], r: Heap[A]): Heap[A] = r match {
    case Branch(y, lt, rt, _, _) => Heap(y, l, Heap(x, lt, rt))
    case _ => Heap(x, l, r)
  }

  implicit def toShowable[A](implicit s: Show[A], order: Order[A]): Show[Heap[A]] = new Show[Heap[A]] {
    override def show(f: Heap[A]): String = f.toList match {
      case Nil => "[]"
      case h :: t => t.foldLeft("[" + s.show(h))((acc, r) => acc + ", " + s.show(r)) + "]"
    }
  }
}
