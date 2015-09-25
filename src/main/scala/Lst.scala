package dogs

import cats._
import cats.implicits._
import scala.annotation.tailrec

/**
 * Immutable, singly-linked list implementation.
 *
 * This code is very similar to scala.List, with a few key differences:
 *
 * 1. It does not expose any "unsafe" methods.
 * 2. It is invariant, whereas scala.List is covariant.
 * 3. It uses subtyping to differentiate non-emptiness.
 * 4. It provides Cats type class instances.
 * 5. It does not currently provide a fast (mutable) builder.
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
sealed abstract class Lst[A] {

  final def isEmpty: Boolean =
    this match {
      case El() => true
      case _ => false
    }

  final def cata[B](b: => B, f: (A, Lst[A]) => B): B =
    this match {
      case Nel(h, t) => f(h, t)
      case El() => b
    }

  final def toNel: Option[Nel[A]] =
    this match {
      case nel: Nel[_] => Some(nel)
      case El() => None
    }

  final def ::(a: A): Nel[A] =
    Nel(a, this)

  @tailrec final def foldLeft[B](b: B)(f: (B, A) => B): B =
    this match {
      case El() => b
      case Nel(h, t) => t.foldLeft(f(b, h))(f)
    }

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def :::(as: Lst[A]): Lst[A]

  def :::(as: Nel[A]): Nel[A]

  def map[B](f: A => B): Lst[B]

  def flatMap[B](f: A => Lst[B]): Lst[B]

  def coflatMap[B](f: Lst[A] => B): Lst[B] = {
    @tailrec def loop(cur: Lst[A], acc: Lst[B]): Lst[B] =
      cur match {
        case n: Nel[_] => loop(n.tail, f(n) :: acc)
        case El() => acc.reverse
      }
    loop(this, Lst.empty)
  }

  def filter(f: A => Boolean): Lst[A]

  @tailrec final def find(p: A => Boolean): Option[A] =
    this match {
      case El() => None
      case Nel(h, t) => if (p(h)) Some(h) else t.find(p)
    }

  final def exists(p: A => Boolean): Boolean =
    find(p).isDefined

  final def forall(p: A => Boolean): Boolean =
    find(a => !p(a)).isEmpty

  final def contains(a: A)(implicit ev: Eq[A]): Boolean =
    find(_ === a).isDefined

  def reverse: Lst[A]

  def take(n: Int): Lst[A]

  @tailrec final def drop(n: Int): Lst[A] =
    if (n <= 0) this else this match {
      case Nel(h, t) => t.drop(n - 1)
      case El() => this
    }

  def traverse[F[_], B](f: A => F[B])(implicit F: Applicative[F]): F[Lst[B]] =
    foldRight(Eval.now(F.pure(Lst.empty[B]))) { (a, lfbs) =>
      lfbs.map(fbs => F.map2(f(a), fbs)(_ :: _))
    }.value

  override def toString: String = {
    def loop(sb: StringBuilder, h: A, t: Lst[A]): String =
      t match {
        case El() =>
          sb.append(h).append(")").toString
        case Nel(th, tt) =>
          loop(sb.append(h).append(", "), th, tt)
      }
    this match {
      case El() => "El()"
      case Nel(h, t) => loop(new StringBuilder("Lst("), h, t)
    }
  }
}

final case class Nel[A](head: A, tail: Lst[A]) extends Lst[A] {

  final def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    f(head, Eval.defer(tail.foldRight(lb)(f)))

  final def reduceLeft(f: (A, A) => A): A =
    tail.foldLeft(head)(f)

  final def :::(as: Lst[A]): Nel[A] =
    as.foldRight(Now(this))((a, lbs) => lbs.map(a :: _)).value

  final def :::(as: Nel[A]): Nel[A] =
    as.foldRight(Now(this))((a, lbs) => lbs.map(a :: _)).value

  final def map[B](f: A => B): Nel[B] = {
    val h = f(head)
    val t = tail.foldRight(Now(Lst.empty[B])) { (a, lbs) =>
      lbs.map(f(a) :: _)
    }.value
    Nel(h, t)
  }

  final def flatMap[B](f: A => Nel[B]): Nel[B] =
    f(head) ::: tail.flatMap(f)

  final def flatMap[B](f: A => Lst[B]): Lst[B] =
    foldRight(Now(Lst.empty[B]))((a, lbs) => lbs.map(f(a) ::: _)).value

  final def coflatMap[B](f: Nel[A] => B): Nel[B] = {
    @tailrec def loop(cur: Lst[A], acc: Nel[B]): Nel[B] =
      cur match {
        case n: Nel[_] => loop(n.tail, f(n) :: acc)
        case El() => acc.reverse
      }
    loop(tail, Lst(f(this)))
  }

  final def filter(f: A => Boolean): Lst[A] =
    foldRight(Now(Lst.empty[A]))((a, lbs) => if (f(a)) lbs.map(a :: _) else lbs).value

  final def reverse: Nel[A] =
    tail.foldLeft(Lst(head))((lst, a) => a :: lst)

  final def take(n: Int): Lst[A] = {
    @tailrec def loop(i: Int, acc: Lst[A], rest: Lst[A]): Lst[A] =
      if (i >= n) acc else rest match {
        case El() => acc
        case Nel(h, t) => loop(i + 1, h :: acc, t)
      }
    loop(0, Lst.empty, reverse)
  }
}

final case class El[A]() extends Lst[A] {
  final def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
  final def :::(as: Lst[A]): Lst[A] = as
  final def :::(as: Nel[A]): Nel[A] = as
  final def map[B](f: A => B): Lst[B] = El[B]
  final def flatMap[B](f: A => Lst[B]): El[B] = El[B]
  final def filter(f: A => Boolean): El[A] = this
  final def take(n: Int): El[A] = this
  final def reverse: El[A] = this
}

object Lst extends LstInstances {
  final def empty[A]: Lst[A] =
    El[A]

  final def apply[A](a: A): Nel[A] =
    Nel(a, El())

  final def apply[A](a1: A, a2: A, as: A*): Nel[A] =
    a1 :: a2 :: fromIterable(as)

  final def fromIterable[A](as: Iterable[A]): Lst[A] =
    as.foldLeft(Lst.empty[A])((lst, a) => a :: lst).reverse
}

trait LstInstances extends LstInstances1 {
  implicit val lstHasMonad: MonadCombine[Lst] with CoflatMap[Lst] =
    new MonadCombine[Lst] with CoflatMap[Lst] {
      def empty[A]: Lst[A] =
        Lst.empty
      def pure[A](a: A): Lst[A] =
        Lst(a)
      override def map[A, B](fa: Lst[A])(f: A => B): Lst[B] =
        fa.map(f)
      def flatMap[A, B](fa: Lst[A])(f: A => Lst[B]): Lst[B] =
        fa.flatMap(f)
      def combine[A](x: Lst[A], y: Lst[A]): Lst[A] =
        x ::: y
      def coflatMap[A, B](fa: Lst[A])(f: Lst[A] => B): Lst[B] =
        fa.coflatMap(f)
    }

  implicit val lstHasTraverse: Traverse[Lst] =
    new Traverse[Lst] {
      def traverse[F[_], A, B](fa: Lst[A])(f: A => F[B])(implicit F: Applicative[F]): F[Lst[B]] =
        fa.traverse(f)

      def foldLeft[A, B](fa: Lst[A], z: B)(f: (B, A) => B) =
        fa.foldLeft(z)(f)

      def foldRight[A, B](fa: Lst[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        fa.foldRight(z)(f)

      override def foldMap[A, B](fa: Lst[A])(f: A => B)(implicit M: Monoid[B]): B =
        fa.foldLeft(M.empty)((b, a) => b |+| f(a))
    }

  implicit def lstOrder[A: Order]: Order[Lst[A]] =
    new Order[Lst[A]] {
      @tailrec final def compare(x: Lst[A], y: Lst[A]): Int =
        x match {
          case Nel(h1, t1) =>
            y match {
              case Nel(h2, t2) =>
                val c = h1 compare h2
                if (c == 0) compare(t1, t2) else c
              case _ => 1
            }
          case _ =>
            if (y.isEmpty) 0 else -1
        }
    }
}

trait LstInstances1 extends LstInstances2 {
  implicit def lstPartialOrder[A: PartialOrder]: PartialOrder[Lst[A]] =
    new PartialOrder[Lst[A]] {
      @tailrec final def partialCompare(x: Lst[A], y: Lst[A]): Double =
        x match {
          case Nel(h1, t1) =>
            y match {
              case Nel(h2, t2) =>
                val c = h1 partialCompare h2
                if (c == 0.0) partialCompare(t1, t2) else c
              case _ => 1.0
            }
          case _ =>
            if (y.isEmpty) 0.0 else -1.0
        }
    }
}

trait LstInstances2 {
  implicit def lstEq[A: Eq]: Eq[Lst[A]] =
    new Eq[Lst[A]] {
      @tailrec final def eqv(x: Lst[A], y: Lst[A]): Boolean =
        (x, y) match {
          case (Nel(h1, t1), Nel(h2, t2)) =>
            h1 === h2 && eqv(t1, t2)
          case _ =>
            x.isEmpty && y.isEmpty
        }
    }
}

object Nel extends NelInstances

trait NelInstances {
  implicit val NelHasBimonad: Bimonad[Nel] =
    new Bimonad[Nel] {
      def pure[A](a: A): Nel[A] =
        Lst(a)
      def extract[A](fa: Nel[A]): A =
        fa.head
      override def map[A, B](fa: Nel[A])(f: A => B): Nel[B] =
        fa.map(f)
      def flatMap[A, B](fa: Nel[A])(f: A => Nel[B]): Nel[B] =
        fa.flatMap(f)
      def coflatMap[A, B](fa: Nel[A])(f: Nel[A] => B): Nel[B] =
        fa.coflatMap(f)
    }

  implicit val NelHasSemigroupK: SemigroupK[Nel] =
    new SemigroupK[Nel] {
      def combine[A](x: Nel[A], y: Nel[A]): Nel[A] = x ::: y
    }
}
