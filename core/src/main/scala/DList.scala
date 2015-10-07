package dogs

import cats._
import cats.Eval._
import Lst._
import Maybe._

import scala.Boolean

case class DList[A](run: Lst[A] => Eval[Lst[A]]) extends DListMethods[A] {

  def apply(tail: Lst[A]): Eval[Lst[A]] = run(tail)

  def +:(a: A): DList[A] = DList(as => defer(apply(as) map (a :: _)))

  def :+(a: A): DList[A] = DList(as => defer(apply(a :: as)))

  def ++(as: => DList[A]): DList[A] =
    DList(tail => defer(as(tail) flatMap apply))
  
}

object DList {
  def empty[A]: DList[A] = DList(now[Lst[A]] _)
  def apply[A](as: Lst[A]): DList[A] = DList(tail => now(as ::: tail))
}

trait DListMethods[A] { self: DList[A] =>

  def no[AA]: Eval[Maybe[AA]] = now(notThere)

  def uncons[B](z: Eval[B], f: (A, DList[A]) => B): B =
    run(empty).flatMap(_.cata(z, (x,l) => now(f(x,DList(l))))).value

  def toLst: Lst[A] = run(empty).value

  /** Get the first element of the list, if any. */
  def headMaybe: Maybe[A] = uncons[Maybe[A]](no,(x, _) => There(x))

  /** Tests whether list is empty. */
  def isEmpty: Boolean = uncons(now(true), (_, _) => false)

  /** Get the tail of the list, if any. */
  def tailMaybe: Maybe[DList[A]] =
    uncons[Maybe[DList[A]]](no, (_, y) => There(y))

  def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    run(empty).flatMap( _.foldRight(b)(f))

  def map[B](f: A => B): DList[B] =
    DList(t => run(empty).map(x => (x map f) ::: t))

  def flatMap[B](f: A => DList[B]): DList[B] =
   foldRight(now(DList.empty[B]))((a,bs) => bs.map(bs => f(a) ++ bs)).value
}

