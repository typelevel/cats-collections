package dogs

import scala.{Boolean,Double,Int,Iterable}
import java.lang.{String,StringBuilder}
import scala.annotation.tailrec

/**
 * Immutable, singly-linked list implementation.
 *
 * This code is very similar to scala.List, with a few key differences:
 *
 * 1. It does not expose any "unsafe" methods.
 * 2. It is invariant, whereas scala.List is covariant.
 * 3. It uses subtyping to differentiate non-emptiness.
 * 4. It does not currently provide a fast (mutable) builder.
 *
 * The types defined here are as follows:
 *
 *  - Lst[A] represents a list of zero-or-more elements.
 *  - Nel[A] represents a list of one-or-more elements.
 *  - El[A]  represents an empty list (exactly zero elements).
 *
 * (Every Lst[A] is either a Nel[A] or an El[A].)
 *
 * While it does not (yet) provide every single Scala collection
 * method, it provides a decent number of them. Type class instances
 * are provided for both Lst and Nel (for example, Nel has a Comonad
 * instance, whereas Lst only has a CoflatMap instance).
 */
sealed abstract class List[A] {
  import Option._

  final def isEmpty: Boolean =
    this match {
      case El() => true
      case _ => false
    }

  final def cata[B](b: => B, f: (A, List[A]) => B): B =
    this match {
      case Nel(h, t) => f(h, t)
      case El() => b
    }

  final def toNel: Option[Nel[A]] =
    this match {
      case nel: Nel[_] => Some(nel)
      case El() => None()
    }

  final def ::(a: A): Nel[A] =
    Nel(a, this)

  @tailrec final def foldLeft[B](b: B)(f: (B, A) => B): B =
    this match {
      case El() => b
      case Nel(h, t) => t.foldLeft(f(b, h))(f)
    }

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def :::(as: List[A]): List[A]

  def :::(as: Nel[A]): Nel[A]

  def map[B](f: A => B): List[B]

  def flatMap[B](f: A => List[B]): List[B]

  def coflatMap[B](f: List[A] => B): List[B] = {
    @tailrec def loop(cur: List[A], acc: List[B]): List[B] =
      cur match {
        case n: Nel[_] => loop(n.tail, f(n) :: acc)
        case El() => acc.reverse
      }
    loop(this, List.empty)
  }

  def filter(f: A => Boolean): List[A]

  @tailrec final def find(p: A => Boolean): Option[A] =
    this match {
      case El() => None()
      case Nel(h, t) => if (p(h)) Some(h) else t.find(p)
    }

  final def exists(p: A => Boolean): Boolean =
    find(p).isSome

  final def forall(p: A => Boolean): Boolean =
    find(a => !p(a)).isNone

/*  final def contains(a: A)(implicit ev: Eq[A]): Boolean =
    find(_ === a).isDefined
*/

  def reverse: List[A]

  def take(n: Int): List[A]

  @tailrec final def drop(n: Int): List[A] =
    if (n <= 0) this else this match {
      case Nel(h, t) => t.drop(n - 1)
      case El() => this
    }

/*
  def traverse[F[_], B](f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
    foldRight(Eval.now(F.pure(List.empty[B]))) { (a, lfbs) =>
      lfbs.map(fbs => F.map2(f(a), fbs)(_ :: _))
    }.value
 */
  override def toString: String = {
    def loop(sb: StringBuilder, h: A, t: List[A]): String =
      t match {
        case El() =>
          sb.append(h).append(")").toString
        case Nel(th, tt) =>
          loop(sb.append(h).append(", "), th, tt)
      }
    this match {
      case El() => "El()"
      case Nel(h, t) => loop(new StringBuilder("List("), h, t)
    }
  }
}

final case class Nel[A](head: A, private[dogs] var _tail: List[A]) extends List[A] {
  def tail = _tail

  final def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    f(head, Eval.defer(tail.foldRight(lb)(f)))

  final def reduceLeft(f: (A, A) => A): A =
    tail.foldLeft(head)(f)

  final def :::(as: List[A]): Nel[A] =
    as.foldRight(Now(this))((a, lbs) => lbs.map(a :: _)).value

  final def :::(as: Nel[A]): Nel[A] =
    as.foldRight(Now(this))((a, lbs) => lbs.map(a :: _)).value

  final def map[B](f: A => B): Nel[B] = {
    val h = f(head)
    val t = tail.foldRight(Now(List.empty[B])) { (a, lbs) =>
      lbs.map(f(a) :: _)
    }.value
    Nel(h, t)
  }

  final def flatMap[B](f: A => Nel[B]): Nel[B] =
    f(head) ::: tail.flatMap(f)

  final def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Now(List.empty[B]))((a, lbs) => lbs.map(f(a) ::: _)).value

  final def coflatMap[B](f: Nel[A] => B): Nel[B] = {
    @tailrec def loop(cur: List[A], acc: Nel[B]): Nel[B] =
      cur match {
        case n: Nel[_] => loop(n.tail, f(n) :: acc)
        case El() => acc.reverse
      }
    loop(tail, List(f(this)))
  }

  final def filter(f: A => Boolean): List[A] =
    foldRight(Now(List.empty[A]))((a, lbs) => if (f(a)) lbs.map(a :: _) else lbs).value

  final def reverse: Nel[A] =
    tail.foldLeft(List(head))((lst, a) => a :: lst)

  final def take(n: Int): List[A] = {
    @tailrec def loop(i: Int, acc: List[A], rest: List[A]): List[A] =
      if (i >= n) acc else rest match {
        case El() => acc
        case Nel(h, t) => loop(i + 1, h :: acc, t)
      }
    loop(0, List.empty, reverse)
  }
}

final case class El[A]() extends List[A] {
  final def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
  final def :::(as: List[A]): List[A] = as
  final def :::(as: Nel[A]): Nel[A] = as
  final def map[B](f: A => B): List[B] = El[B]
  final def flatMap[B](f: A => List[B]): El[B] = El[B]
  final def filter(f: A => Boolean): El[A] = this
  final def take(n: Int): El[A] = this
  final def reverse: El[A] = this
}

object List {
  final def empty[A]: List[A] =
    El[A]

  final def apply[A](a: A): Nel[A] =
    Nel(a, El())

  final def apply[A](a1: A, a2: A, as: A*): Nel[A] =
    a1 :: a2 :: fromIterable(as)

  final def fromIterable[A](as: Iterable[A]): List[A] =
    as.foldLeft(List.empty[A])((lst, a) => a :: lst).reverse

  def fill[A](n: Int)(a: => A): List[A] = {
    @tailrec
    def go(n: Int, a: => A, l: List[A]): List[A] =
      if(n > 0) go(n-1, a, a :: l) else l

    go(n, a, El())
  }
}

class ListBuilder[A] {
  var run: List[A] = El()
  var end: Nel[A] = _

  def +=(a: A) = {
    run match {
      case El() =>
        end = Nel(a, El())
        run = end
      case _ =>
        val newEnd = Nel(a, El())
        end._tail = newEnd
        end = newEnd
    }
    this
  }

  def nonEmpty: Boolean = run != El()
  def isEmpty: Boolean = run == El()
  def toList:List[A] = run
}
