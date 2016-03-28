package dogs

import algebra.Order
import dogs.Heap.Leaf

/**
 * Created by nperez on 3/28/16.
 */

sealed abstract class Heap[A] {


  def min: A

  def left: Heap[A]

  def right: Heap[A]

  def size: Int

  def height: Int

  def isEmpty: Boolean


  /// O(log n)
  def insert(x: A)(implicit order: Order[A]): Heap[A] =
    if (isEmpty)
      Heap(x)
    else if (left.size < math.pow(2, left.height) - 1)
      Heap.bubbleUp(min, left.insert(x), right)
    else if (right.size < math.pow(2, right.height) - 1)
      Heap.bubbleUp(min, left, right.insert(x))
    else if (right.height < left.height)
      Heap.bubbleUp(min, left, right.insert(x))
    else
      Heap.bubbleUp(min, left.insert(x), right)
}

object Heap {
  def apply[A](x: A, l: Heap[A] = Leaf, r: Heap[A] = Leaf): Heap[A] =
    Branch(x, l, r, l.size + r.size + 1, math.max(l.height, r.height) + 1)

  case class Branch[A](min: A, left: Heap[A], right: Heap[A], size: Int, height: Int) extends Heap[A] {
    override def isEmpty: Boolean = false
  }

  case object Leaf extends Heap[Option[Nothing]] {
    def unapply[A](heap: Heap[A]): Boolean = heap.isEmpty

    override def min: Option[Nothing] = None()

    override def size: Int = 0

    override def height: Int = 0

    override def left: Heap[Option[Nothing]] = Leaf

    override def right: Heap[Option[Nothing]] = Leaf

    override def isEmpty: Boolean = true
  }

  def bubbleUp[A](x: A, l: Heap[A], r: Heap[A])(implicit order: Order[A]): Heap[A] = (l, r) match {
    case (Branch(y, lt, rt, _, _), _)   if order.gt(x, y)     =>  Heap(y, Heap(x, lt, rt), r)
    case (_, Branch(z, lt, rt), r)      if order.gt(x, z)     =>  Heap(z, Heap(x, lt, rt))
    case (_, _)                                               =>  Heap(x, l, r)
  }
}
