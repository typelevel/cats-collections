package cats.collections.laws

import cats.Order
import cats.collections.PartiallyOrderedSet
import cats.laws.{IsEq, IsEqArrow, UnorderedFoldableLaws}
import cats.kernel.CommutativeMonoid

import cats.implicits._

/**
 * We test all default methods against this proxy so overrides can never change semantics
 */
class MinimalPartialOrderedSetProxy[F[_]](pos: PartiallyOrderedSet[F]) extends PartiallyOrderedSet[F] {
  def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(fn: A => B): B =
    pos.unorderedFoldMap(fa)(fn)

  def add[A](fa: F[A], a: A)(implicit order: Order[A]): F[A] =
    pos.add(fa, a)

  def contains[A: Order](fa: F[A], a: A): Boolean =
    pos.contains(fa, a)

  def empty[A]: F[A] = pos.empty

  def minimumOption[A](fa: F[A]): Option[A] =
    pos.minimumOption(fa)

  def removeMin[A](fa: F[A])(implicit order: Order[A]): F[A] =
    pos.removeMin(fa)

  def singleton[A](a: A): F[A] =
    pos.singleton(a)
}

trait PartiallyOrderedSetLaws[F[_]] extends UnorderedFoldableLaws[F] {
  implicit override def F: PartiallyOrderedSet[F]

  private def proxy: PartiallyOrderedSet[F] = new MinimalPartialOrderedSetProxy(F)

  // This should be on UnorderdFoldable but was omitted
  def sizeMatchesUnorderedFoldMap[A](fa: F[A]): Boolean =
    F.size(fa) == F.unorderedFoldMap(fa)(_ => 1L)

  def addEmptyIsSingleton[A: Order](a: A): IsEq[F[A]] =
    F.add(F.empty[A], a) <-> F.singleton(a)

  def addMonoidConsistency[A: Order, B: CommutativeMonoid](fa: F[A], a: A, fn: A => B): IsEq[B] =
    F.unorderedFoldMap(F.add(fa, a))(fn) <-> (CommutativeMonoid[B].combine(F.unorderedFoldMap(fa)(fn), fn(a)))

  def emptyIsEmpty[A: CommutativeMonoid]: IsEq[A] =
    F.unorderedFold(F.empty[A]) <-> CommutativeMonoid[A].empty

  def addIncreasesSize[A: Order](fa: F[A], a: A): Boolean =
    F.size(fa) == (F.size(F.add(fa, a)) - 1L)

  def containsMatchesToList[A: Order](fa: F[A], a: A): Boolean =
    F.contains(fa, a) == (F.toSortedList(fa).exists { aa => Order[A].eqv(aa, a) })

  def removeDecreasesSize[A: Order](fa: F[A]): Boolean =
    F.isEmpty(fa) || (F.size(fa) == (F.size(F.removeMin(fa)) + 1L))

  def minimumOptionIsTheMinimum[A: Order](fa: F[A]): Boolean = {
    val mo = F.minimumOption(fa)
    val listMo = F.toSortedList(fa).headOption
    Order[Option[A]].eqv(mo, listMo)
  }

  def removeMinNeverDecreasesMin[A: Order](fa: F[A]): Boolean = {
    val m1 = F.minimumOption(fa)
    val fa2 = F.removeMin(fa)
    val m2 = F.minimumOption(fa2)
    (m1, m2) match {
      case (None, None) => F.isEmpty(fa) && F.isEmpty(fa2)
      case (None, Some(_)) => false // this should never happen
      case (Some(_), None) => F.isEmpty(fa2)
      case (Some(a), Some(b)) => Order[A].lteqv(a, b)
    }
  }

  /*
   * The rest of these test that semantics of methods didn't change from overrides
   */

  def addAllListSameAsSort[A: Order](ls: List[A]): IsEq[List[A]] =
    ls.sorted <-> F.toSortedList(F.build(ls))

  def addAllMatchesProxy[A: Order](fa: F[A], items: Iterable[A]): IsEq[F[A]] =
    F.addAll(fa, items) <-> proxy.addAll(fa, items)

  def buildMatchesProxy[A: Order](items: Iterable[A]): IsEq[F[A]] =
    F.build(items) <-> proxy.build(items)

  def sortedFoldLeftMatchesProxy[A: Order, B](fa: F[A], init: B, fn: (B, A) => B): IsEq[B] =
    F.sortedFoldLeft(fa, init)(fn) <-> proxy.sortedFoldLeft(fa, init)(fn)

  def takeLargestMatchesProxy[A: Order](fa: F[A], maxSize: Long): IsEq[F[A]] =
    F.takeLargest(fa, maxSize) <-> proxy.takeLargest(fa, maxSize)

  def addIfLargerMatchesProxy[A: Order](fa: F[A], maxSize: Long, item: A): IsEq[F[A]] =
    F.addIfLarger(fa, maxSize, item) <-> proxy.addIfLarger(fa, maxSize, item)

  def toSortedListMatchesProxy[A: Order](fa: F[A]): IsEq[List[A]] =
    F.toSortedList(fa) <-> proxy.toSortedList(fa)

  def addAllLargestMatchesProxy[A: Order](fa: F[A], maxSize: Long, items: Iterable[A]): IsEq[F[A]] =
    F.addAllLargest(fa, maxSize, items) <-> proxy.addAllLargest(fa, maxSize, items)

  def unaddMatchesProxy[A: Order](fa: F[A]): IsEq[Option[(A, F[A])]] =
    F.unadd(fa) <-> proxy.unadd(fa)
}
