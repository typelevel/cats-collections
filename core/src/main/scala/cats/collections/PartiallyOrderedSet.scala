package cats.collections

import cats.{UnorderedFoldable, Order}
import scala.annotation.tailrec

/**
 * This is a typeclass for Heap-like data-structures which are not totally ordered sets,
 * but can produce the minimimum value, add and remove items.
 */
trait PartiallyOrderedSet[F[_]] extends UnorderedFoldable[F] {

  /**
   * Add an item to fa
   */
  def add[A](fa: F[A], a: A)(implicit order: Order[A]): F[A]

  /**
   * use the heap property to be more efficient than toSortedList to check containment
   */
  def contains[A](fa: F[A], a: A)(implicit order: Order[A]): Boolean

  /**
   * return an empty F
   */
  def empty[A]: F[A]

  /**
   * Get the minimum value from fa if not empty
   */
  def minimumOption[A](fa: F[A]): Option[A]

  /**
   * Remove the minimum value if fa is not empty, else return empty
   */
  def removeMin[A](fa: F[A])(implicit order: Order[A]): F[A]

  /**
   * make an f with a single item
   */
  def singleton[A](a: A): F[A]

  ////////
  // The following are not fundamental, but may be optimized by implementations
  ////////

  def addAll[A](fa: F[A], items: Iterable[A])(implicit order: Order[A]): F[A] =
    items.foldLeft(fa)(add(_, _))

  /**
   * build an F from Iterable
   */
  def build[A](items: Iterable[A])(implicit order: Order[A]): F[A] =
    addAll(empty[A], items)

  /**
   * fold in order
   */
  def sortedFoldLeft[A, B](fa: F[A], init: B)(fn: (B, A) => B)(implicit order: Order[A]): B = {
    @tailrec def loop(fa: F[A], init: B): B =
      minimumOption(fa) match {
        case None => init
        case Some(min) => loop(removeMin(fa), fn(init, min))
      }

    loop(fa, init)
  }

  /**
   * remove as many as needed until size <= maxSize
   */
  def takeLargest[A](fa: F[A], maxSize: Long)(implicit order: Order[A]): F[A] =
    if (maxSize <= 0) empty[A]
    else {
      var surplus = size(fa) - maxSize
      var res = fa
      while (surplus > 0) {
        res = removeMin(res)
        surplus -= 1
      }
      res
    }

  /**
   * same as takeLargest(add(fa, item), maxSize)
   */
  def addIfLarger[A](fa: F[A], maxSize: Long, item: A)(implicit order: Order[A]): F[A] =
    if (maxSize <= 0) empty[A]
    else {
      val sz = size(fa)
      if (sz < maxSize) add(fa, item)
      else if (order.lt(minimumOption(fa).get, item)) add(removeMin(fa), item)
      else fa
    }

  /**
   * return a sorted list of all items in fa
   */
  def toSortedList[A](fa: F[A])(implicit order: Order[A]): List[A] =
    sortedFoldLeft(fa, List.empty[A]) { (tail, a) => a :: tail }.reverse

  /**
   * same as items.foldLeft(fa)(addIfLarger(_, count, _))
   */
  def addAllLargest[A](fa: F[A], maxSize: Long, items: Iterable[A])(implicit order: Order[A]): F[A] =
    items.foldLeft(fa)(addIfLarger(_, maxSize, _))

  /**
   * Same as get the minimimumOption and removinging the minimum
   */
  def unadd[A](fa: F[A])(implicit order: Order[A]): Option[(A, F[A])] =
    minimumOption(fa).map { min => (min, removeMin(fa)) }

  /**
   * Given an Order[A] we can always make an Order[F[A]]
   */
  def order[A: Order]: Order[F[A]] =
    new Order[F[A]] {
      val ordA: Order[A] = Order[A]

      @tailrec
      final def compare(left: F[A], right: F[A]): Int =
        (minimumOption(left), minimumOption(right)) match {
          case (None, None) => 0
          case (None, Some(_)) => -1
          case (Some(_), None) => 1
          case (Some(l), Some(r)) =>
            val c = ordA.compare(l, r)
            if (c != 0) c
            else compare(removeMin(left), removeMin(right))
      }
    }
}

object PartiallyOrderedSet {
  def apply[F[_]](implicit pos: PartiallyOrderedSet[F]): PartiallyOrderedSet[F] = pos
}
