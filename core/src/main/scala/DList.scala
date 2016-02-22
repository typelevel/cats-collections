package dogs

import Predef._
import List._
import Option._
import Eval._

/**
 * A "difference list" - A List like structure with O(1) appends.
 * Based on `Data.DList`, a Haskell library by Don Stewart.
 */
final class DList[A](val run: List[A] => Eval[List[A]]) {

  /**
   * Prepend the contents of this dlist to the given tail.
   */
  def apply(tail: List[A]): Eval[List[A]] = run(tail)

  /**
   * Prepend a single element to this DList
   * O(1)
   */
  def +:(a: A): DList[A] = new DList(t => defer(apply(t)) map (a :: _))

  /**
   * append a single element to this DList
   * O(1)
   */
  def :+(a: A): DList[A] = this ++ DList(List(a))

  /**
   * Append a list to this DList
   * O(1)
   */
  def ++(as: DList[A]): DList[A] =
    new DList(tail => as(tail) flatMap apply)

  /**
   * Destructure the DList into a head and a tail, and pass both to a
   * function
   * O(n) where n is the number of append operations in this DList
   */
  def uncons[B](z: Eval[B], f: (A, DList[A]) => Eval[B]): Eval[B] =
    run(List.empty) flatMap {
      case El() => z
      case h Nel t => f(h, DList(t))
    }

  /**
   * O(n)
   */
  def toList: List[A] = run(empty).value

  /** 
   * Get the first element of the list, if any. 
   * O(n) where n is the number of append operations in this DList
   */
  def headOption: Option[A] = uncons[Option[A]](now(none),(x, _) => now(Some(x))).value

  /** 
   * Get the tail of the list, if any. 
   * O(n) where n is the number of append operations in this DList
   */
  def tailOption: Option[DList[A]] =
    uncons[Option[DList[A]]](now(none), (_, y) => now(Some(y))).value

  /** 
   * Tests whether list is empty.
   * O(n) where n is the number of append operations in this DList
   * which is a bit weirdly expensive
   */
  def isEmpty: Boolean = headOption.isNone

  def foldr[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    run(empty).flatMap( _.foldRight(b)(f))

  def map[B](f: A => B): DList[B] =
    new DList(t => run(empty).map(x => (x map f) ::: t))

  def flatMap[B](f: A => DList[B]): DList[B] =
   foldr(now(DList.empty[B]))((a,bs) => bs.map(bs => f(a) ++ bs)).value
}

object DList {
  def empty[A]: DList[A] = new DList(t => defer(now(t)))
  def apply[A](as: List[A]): DList[A] = new DList(tail => defer(now(as ::: tail)))
}

