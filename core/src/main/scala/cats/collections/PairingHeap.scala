package cats.collections

import cats.{Order, UnorderedFoldable, Show}
import cats.kernel.CommutativeMonoid
import scala.annotation.tailrec

/**
 * A PairingHeap is a heap with excellent empirical performance.
 *
 * See:
 * https://en.wikipedia.org/wiki/Pairing_heap
 * in particular:
 * https://en.wikipedia.org/wiki/Pairing_heap#Summary_of_running_times
 *
 * Additionally, it supports an efficient O(1) combine operation
 */
sealed abstract class PairingHeap[A] {

  import PairingHeap._

  /**
   * insert an item into the heap
   * O(1)
   */
  def add(x: A)(implicit order: Order[A]): PairingHeap[A] =
    if (this.isEmpty) apply(x)
    else {
      val thisTree = this.asInstanceOf[Tree[A]]
      if (order.lteqv(x, thisTree.min)) Tree(x, this :: Nil)
      else Tree(thisTree.min, Tree(x, Nil) :: thisTree.subtrees)
    }

  /**
   * Returns min value on the heap, if it exists
   *
   * O(1)
   */
  def minimumOption: Option[A]

  /**
   * Returns the size of the heap.
   * O(1)
   */
  def size: Long

  /**
   * Verifies if the heap is empty.
   * O(1)
   */
  def isEmpty: Boolean

  /**
   * Return true if this is not empty
   * O(1)
   */
  def nonEmpty: Boolean = !isEmpty

  /**
   * Returns a sorted list of the elements within the heap.
   * O(N log N)
   */
  def toList(implicit order: Order[A]): List[A] = {
    @tailrec
    def loop(h: PairingHeap[A], acc: List[A]): List[A] =
      h match {
        case Tree(m, _) => loop(h.remove, m :: acc)
        case Leaf()          => acc.reverse
      }

    loop(this, Nil)
  }

  /**
   * Merge two heaps, this is O(1) work
   */
  def combine(that: PairingHeap[A])(implicit order: Order[A]): PairingHeap[A]

  /**
   * Check to see if a predicate is ever true
   * worst case O(N) but stops at the first true
   */
  def exists(fn: A => Boolean): Boolean

  /**
   * Check to see if a predicate is always true
   * worst case O(N) but stops at the first false
   */
  def forall(fn: A => Boolean): Boolean

  /**
   * Aggregate with a commutative monoid, since the Heap is not totally
   * ordered
   */
  def unorderedFoldMap[B](fn: A => B)(implicit m: CommutativeMonoid[B]): B =
    if (isEmpty) m.empty
    else {
      val thisTree = this.asInstanceOf[Tree[A]]
      m.combine(fn(thisTree.min), m.combineAll(thisTree.subtrees.map(_.unorderedFoldMap(fn))))
    }

  /**
   * Similar to unorderedFoldMap without a transformation
   */
  def unorderedFold(implicit m: CommutativeMonoid[A]): A =
    if (isEmpty) m.empty
    else {
      val thisTree = this.asInstanceOf[Tree[A]]
      m.combine(thisTree.min, m.combineAll(thisTree.subtrees.map(_.unorderedFold)))
    }

  /**
   * do a foldLeft in the same order as toList.
   * requires an Order[A], which prevents us from making a Foldable[PairingHeap] instance.
   *
   * prefer unorderedFoldMap if you can express your operation as a commutative monoid
   * since it is O(N) vs O(N log N) for this method
   */
  def foldLeft[B](init: B)(fn: (B, A) => B)(implicit order: Order[A]): B = {
    @tailrec
    def loop(h: PairingHeap[A], acc: B): B =
      h match {
        case Tree(m, _) => loop(h.remove, fn(acc, m))
        case Leaf()          => acc
      }

    loop(this, init)
  }

  /**
   * if not empty, remove the min, else return empty
   * this is thought to be O(log N) (but not proven to be so)
   */
  def remove(implicit order: Order[A]): PairingHeap[A] =
    if (isEmpty) this
    else {
      val thisTree = this.asInstanceOf[Tree[A]]
      combineAll(thisTree.subtrees)
    }

  /**
   * Alias for add
   */
  def +(x: A)(implicit order: Order[A]): PairingHeap[A] = add(x)

  private[collections] def subtrees: List[PairingHeap[A]]
}

object PairingHeap {

  def empty[A]: PairingHeap[A] = Leaf()

  def apply[A](x: A): PairingHeap[A] = Tree(x, Nil)

  /**
   * This is thought to be O(log N) where N is the size of the final heap
   */
  def combineAll[A: Order](it: Iterable[PairingHeap[A]]): PairingHeap[A] =
    combineAllIter(it.iterator, Nil)

  @tailrec
  private def combineLoop[A: Order](ts: List[PairingHeap[A]], acc: PairingHeap[A]): PairingHeap[A] =
    ts match{
      case Nil => acc
      case h :: tail => combineLoop(tail, h.combine(acc))
    }

  @tailrec
  private def combineAllIter[A: Order](iter: Iterator[PairingHeap[A]], tail: List[PairingHeap[A]]): PairingHeap[A] =
    if (iter.isEmpty) {
      combineLoop(tail, empty[A])
    }
    else {
      val p0 = iter.next()
      if (iter.isEmpty) combineLoop(tail, p0)
      else {
        val p1 = iter.next()
        combineAllIter(iter, p1 :: p0 :: tail)
      }
    }

  // this is a combineall that only counts the number of combines is needs
  // to do for testing
  // this should be kept in sync with combineAllIter
  private[collections] def combineAllIterCount[A: Order](iter: Iterator[PairingHeap[A]], cnt: Int): Int =
    if (iter.isEmpty) cnt
    else {
      iter.next()
      if (iter.isEmpty) cnt + 1
      else {
        iter.next()
        combineAllIterCount(iter, cnt + 2)
      }
    }

  /**
   * build a heap from a list of items, O(N)
   */
  def fromIterable[A](as: Iterable[A])(implicit order: Order[A]): PairingHeap[A] = {
    val iter = as.iterator
    var heap = empty[A]
    while(iter.hasNext) {
      heap = heap + iter.next()
    }
    heap
  }

  /**
   * this is useful for finding the k maximum values in O(N) times for N items
   * same as as.toList.sorted.reverse.take(count), but O(N log(count)) vs O(N log N)
   * for a full sort. When N is very large, this can be a very large savings
   */
  def takeLargest[A](as: Iterable[A], count: Int)(implicit order: Order[A]): PairingHeap[A] =
    if (count <= 0) empty
    else {
      var heap = empty[A]
      val iter = as.iterator
      while (iter.hasNext) {
        val a = iter.next()
        heap =
          if (heap.size < count) heap + a
          else if (order.lt(heap.asInstanceOf[Tree[A]].min, a)) heap.remove + a
          else heap
      }

      heap
    }

  private case class Tree[A](min: A, subtrees: List[PairingHeap[A]]) extends PairingHeap[A] {
    override val size = {
      @tailrec
      def loop(ts: List[PairingHeap[A]], acc: Long): Long =
        ts match {
          case Nil => acc
          case h :: tail => loop(tail, acc + h.size)
        }
      loop(subtrees, 1L)
    }

    override def isEmpty: Boolean = false

    override def minimumOption: Option[A] = Some(min)

    override def exists(fn: A => Boolean): Boolean =
      fn(min) || {
        @tailrec
        def loop(hs: List[PairingHeap[A]]): Boolean =
          hs match {
            case h :: tail => h.exists(fn) || loop(tail)
            case Nil => false
          }
        loop(subtrees)
      }

    override def forall(fn: A => Boolean): Boolean =
      fn(min) && {
        @tailrec
        def loop(hs: List[PairingHeap[A]]): Boolean =
          hs match {
            case h :: tail => h.forall(fn) && loop(tail)
            case Nil => true
          }
        loop(subtrees)
      }

    override def combine(that: PairingHeap[A])(implicit order: Order[A]): PairingHeap[A] =
      if (that.isEmpty) this
      else {
        val thatTree = that.asInstanceOf[Tree[A]]
        if (order.lt(min, thatTree.min)) Tree(min, that :: subtrees)
        else Tree(thatTree.min, this :: thatTree.subtrees)
      }
  }

  private case object Leaf extends PairingHeap[Nothing] {
    def apply[A](): PairingHeap[A] = this.asInstanceOf[PairingHeap[A]]

    def unapply[A](heap: PairingHeap[A]): Boolean = heap.isEmpty

    override def subtrees: List[PairingHeap[Nothing]] = Nil

    override def size: Long = 0L

    override def isEmpty: Boolean = true

    override def minimumOption: Option[Nothing] = None

    override def exists(fn: Nothing => Boolean): Boolean = false

    override def forall(fn: Nothing => Boolean): Boolean = true

    override def combine(that: PairingHeap[Nothing])(implicit order: Order[Nothing]): PairingHeap[Nothing] = that
  }

  implicit def toShowable[A](implicit s: Show[A], order: Order[A]): Show[PairingHeap[A]] = new Show[PairingHeap[A]] {
    override def show(f: PairingHeap[A]): String = {
      val sb = new java.lang.StringBuilder
      sb.append("PairingHeap(")
      f.foldLeft(false) { (notFirst, a) =>
        if (notFirst) sb.append(", ")
        sb.append(s.show(a))
        true
      }
      sb.append(")")
      sb.toString
    }
  }

  implicit val catsCollectionPairingHeapUnorderedFoldable: UnorderedFoldable[PairingHeap] =
    new UnorderedFoldable[PairingHeap] {
      def unorderedFoldMap[A, B: CommutativeMonoid](ha: PairingHeap[A])(fn: A => B): B =
        ha.unorderedFoldMap(fn)

      override def unorderedFold[A: CommutativeMonoid](ha: PairingHeap[A]): A =
        ha.unorderedFold

      override def isEmpty[A](h: PairingHeap[A]) = h.isEmpty
      override def nonEmpty[A](h: PairingHeap[A]) = h.nonEmpty
      override def exists[A](ha: PairingHeap[A])(fn: A => Boolean) = ha.exists(fn)
      override def forall[A](ha: PairingHeap[A])(fn: A => Boolean) = ha.forall(fn)
      override def size[A](h: PairingHeap[A]) = h.size
    }

  private[collections] class PairingHeapOrder[A](implicit ordA: Order[A]) extends Order[PairingHeap[A]] {
    @tailrec
    final def compare(left: PairingHeap[A], right: PairingHeap[A]): Int =
      if (left.isEmpty) {
        if (right.isEmpty) 0
        else -1
      }
      else if (right.isEmpty) 1
      else {
        val lt = left.asInstanceOf[Tree[A]]
        val rt = right.asInstanceOf[Tree[A]]
        val c = ordA.compare(lt.min, rt.min)
        if (c != 0) c
        else compare(left.remove, right.remove)
      }
  }
  /**
   * This is the same order as you would get by doing `.toList` and ordering by that
   */
  implicit def catsCollectionPairingHeapOrder[A: Order]: Order[PairingHeap[A]] =
    new PairingHeapOrder[A]


  implicit def catsCollectionPairingHeapCommutativeMonoid[A: Order]: CommutativeMonoid[PairingHeap[A]] =
    new CommutativeMonoid[PairingHeap[A]] {
      def empty = PairingHeap.empty[A]
      def combine(a: PairingHeap[A], b: PairingHeap[A]) = a.combine(b)
      override def combineAll(as: TraversableOnce[PairingHeap[A]]): PairingHeap[A] =
        combineAllIter(as.toIterator, Nil)
    }
}
