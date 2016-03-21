package dogs

import Predef._
import List._
import Option._
import cats._, cats.Eval._

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

  def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    run(empty).flatMap( _.foldRight(b)(f))

  def map[B](f: A => B): DList[B] =
    new DList(t => run(empty).flatMap(x => (x map f).foldRight(now(t))((a,as) => as.map(a :: _))))

  def flatMap[B](f: A => DList[B]): DList[B] =
    foldRight(now(DList.empty[B]))((a,bs) => defer(bs.map(bs => f(a) ++ bs))).value
}

object DList extends DListInstances{
  def empty[A]: DList[A] = new DList(t => now(t))
  def apply[A](as: List[A]): DList[A] = new DList(tail => as.foldRight(now(tail))((a,as) => as.map(a :: _)))
}

trait DListInstances {
  implicit def dlistMonoid[A]: Monoid[DList[A]] = new Monoid[DList[A]] {
    override def empty: DList[A] = DList.empty
    override def combine(l: DList[A], r: DList[A]): DList[A] = l ++ r
  }

  implicit def dlistEq[A](implicit A: Eq[A]): Eq[DList[A]] =
    Eq[List[A]].on[DList[A]](_.toList)

  implicit val dlistInstance: MonadCombine[DList] with Traverse[DList] =
    new MonadCombine[DList] with Traverse[DList] {
      override def pure[A](a: A): DList[A] =
        DList.empty :+ a

      override def map[A,B](fa: DList[A])(f: A => B): DList[B] = fa map f

      override def flatMap[A,B](fa: DList[A])(f: A => DList[B]): DList[B] = 
        try {
          fa flatMap f
        } catch {
          case e: java.lang.Throwable =>
            e.printStackTrace
            throw e
        }

      override def traverse[G[_], A, B](fa: DList[A])(f: A => G[B])(implicit G: Applicative[G]): G[DList[B]] =
        foldLeft[A, G[DList[B]]](fa,G.pure(DList.empty))((res, a) =>
          G.map2(res, f(a))(_ :+ _)
        )

      override def foldLeft[A, B](fa: dogs.DList[A],b: B)(f: (B, A) => B): B =
        fa.toList.foldLeft(b)(f)

      override def foldRight[A, B](fa: dogs.DList[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = fa.foldRight(lb)(f)

      override def empty[A]: dogs.DList[A] = DList.empty

      override def combineK[A](x: dogs.DList[A],y: dogs.DList[A]): dogs.DList[A] = x ++ y
    }
}
