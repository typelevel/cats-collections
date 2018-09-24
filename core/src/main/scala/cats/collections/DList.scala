package cats.collections

import cats._, cats.implicits._

/**
 * A "difference list" - A List like structure with O(1) appends.
 * Based on `Data.DList`, a Haskell library by Don Stewart.
 */
@deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
final class DList[A](val run: List[A] => Eval[List[A]]) {
  import Eval._

  /**
   * Prepend the contents of this dlist to the given tail.
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def apply(tail: List[A]): Eval[List[A]] = run(tail)

  /**
   * Prepend a single element to this DList
   * O(1)
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def +:(a: A): DList[A] = new DList(t => Eval.defer(run(t)) map (a :: _))

  /**
   * append a single element to this DList
   * O(1)
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def :+(a: A): DList[A] = new DList(t => Eval.defer(run(a :: t)))

  /**
   * Append a list to this DList
   * O(1)
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def ++(as: DList[A]): DList[A] =
    new DList(tail => Eval.defer(as(tail) flatMap run))

  /**
   * Destructure the DList into a head and a tail, and pass both to a
   * function
   * O(n) where n is the number of append operations in this DList
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def uncons[B](z: Eval[B], f: (A, DList[A]) => Eval[B]): Eval[B] =
    run(List.empty) flatMap {
      case Nil => z
      case h :: t => f(h, DList(t))
    }

  /**
   * O(n)
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def toList: List[A] = run(List.empty).value

  /**
   * Get the first element of the list, if any.
   * O(n) where n is the number of append operations in this DList
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def headOption: Option[A] = uncons[Option[A]](now(None),(x, _) => now(Some(x))).value

  /**
   * Get the tail of the list, if any.
   * O(n) where n is the number of append operations in this DList
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def tailOption: Option[DList[A]] =
    uncons[Option[DList[A]]](now(None), (_, y) => now(Some(y))).value

  /**
   * Tests whether list is empty.
   * O(n) where n is the number of append operations in this DList
   * which is a bit weirdly expensive
   */
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def isEmpty: Boolean = headOption.isEmpty

  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    run(List.empty).flatMap(Foldable[List].foldRight(_,b)(f))

  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def map[B](f: A => B): DList[B] =
    new DList(t => run(List.empty).flatMap(x => (x map f).foldRight(now(t))((a,as) => as.map(a :: _))))

  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def flatMap[B](f: A => DList[B]): DList[B] =
    foldRight(now(DList.empty[B]))((a,bs) => bs.map(bs => f(a) ++ bs)).value
}

@deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
object DList extends DListInstances{
  import Eval._

  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def empty[A]: DList[A] = new DList(t => now(t))

  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def apply[A](a: A): DList[A] = new DList(tail => Eval.now(a :: tail))

  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  def apply[A](as: List[A]): DList[A] = new DList(tail => as.foldRight(now(tail))((a,as) => as.map(a :: _)))
}

@deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
trait DListInstances {
  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  implicit def dlistMonoid[A]: Monoid[DList[A]] = new Monoid[DList[A]] {
    override def empty: DList[A] = DList.empty
    override def combine(l: DList[A], r: DList[A]): DList[A] = l ++ r
  }

  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  implicit def dlistEq[A](implicit A: Eq[A]): Eq[DList[A]] =
    Eq[List[A]].contramap[DList[A]](_.toList)

  @deprecated("DLists are superseded by cats.data.Chain", "cats-collections 0.7.0")
  implicit val dlistInstance: Traverse[DList] with MonoidK[DList] = // TODO coflatmap?
    new Traverse[DList] with MonoidK[DList] {
      override def map[A,B](fa: DList[A])(f: A => B): DList[B] = fa map f

      override def traverse[G[_], A, B](fa: DList[A])(f: A => G[B])(implicit G: Applicative[G]): G[DList[B]] = {
        fa.foldRight(Now(G.pure(DList.empty[B]))){ (a, lgsb) => G.map2Eval(f(a), lgsb)(_ +: _) }.value
      }
      override def foldLeft[A, B](fa: cats.collections.DList[A],b: B)(f: (B, A) => B): B =
        fa.toList.foldLeft(b)(f)

      override def foldRight[A, B](fa: cats.collections.DList[A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)

      override def empty[A]: cats.collections.DList[A] = DList.empty

      override def combineK[A](x: cats.collections.DList[A],y: cats.collections.DList[A]): cats.collections.DList[A] = x ++ y
    }
}
