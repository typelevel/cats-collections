package dogs

import cats._

/**
 * An intensional set, which is a set which instead of enumerating its
 * elements as a extensional set does, it is defined by a predicate
 * which is a test for membership.
 */
abstract class ISet[-A] extends scala.Function1[A, Boolean] { self =>
  /**
   * returns a set which is the union of this set and another
   */
  def union[B <: A](other: ISet[B]): ISet[B] = ISet(a => apply(a) || other(a))

  /**
   * returns a set which is the union of this set and another
   */
  def |[B <: A](other: ISet[B]): ISet[B] = ISet(a => apply(a) || other(a))

  /**
   * returns a set which is the intersection of this set and another
   */
  def intersection[B <: A](other: ISet[B]): ISet[B] = ISet(a => apply(a) && other(a))

  /**
   * returns a set which is the intersection of this set and another
   */
  def &[B <: A](other: ISet[B]): ISet[B] = ISet(a => apply(a) && other(a))

  /**
   * Returns true if the value is a member of the set.
   */
  def contains(a: A): Boolean = apply(a)

  /**
   * Returns the set which is the the difference of another set removed from this set
   */
  def diff[B <: A](remove: ISet[B]): ISet[B] = ISet(a => apply(a) && !remove(a))

  /**
   * Returns the set which is the the difference of another set removed from this set
   */
  def -[B <: A](remove: ISet[B]): ISet[B] = ISet(a => apply(a) && !remove(a))

  /**
   * Return the set of all As which are not in this set.
   */
  def negate: ISet[A] = ISet(a => !apply(a))

  /**
   * Return the set of all As which are not in this set.
   */
  def unary_!(): ISet[A] = negate
}

object ISet extends ISetInstances {
  def apply[A](f: A => Boolean): ISet[A] = new ISet[A] {
    def apply(a: A) = f(a)
  }

  def empty: ISet[Any] = apply(_ => false)
}

trait ISetInstances {
  implicit def isetMonoid[A]: Monoid[ISet[A]] = new Monoid[ISet[A]] {
    override def empty: ISet[A] = ISet.empty
    override def combine(l: ISet[A], r: ISet[A]): ISet[A] = l union r
  }

  implicit val isetInstance: MonoidK[ISet] = new MonoidK[ISet] {
    override def empty[A]: ISet[A] = ISet.empty
    override def combineK[A](l: ISet[A], r: ISet[A]): ISet[A] = l union r
  }
}
