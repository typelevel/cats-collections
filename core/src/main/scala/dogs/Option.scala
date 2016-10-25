package dogs

import Predef._
import scala.util.control.NonFatal
import cats._
import cats.data._

/** An optional value
 *
 * An `Option[A]` will either be a wrapped `A` instance (`Some[A]`),
 * or a lack of underlying `A` instance (`None[A]`).
 *
 * `Option[A]` is isomorphic to the Option in the Scala standard
 * library, however there are some differences between the two. This
 * `Option` is invariant in `A` while the standard library `Option` is
 * covariant. This `Option` does not expose an unsafe `get` operation
 * to access the underlying `A` value (that may not exist) like the
 * standard library version does. This `Option` does not come with an
 * implicit conversion to `Iterable` (a trait with over a dozen super
 * types).
 */
sealed abstract class Option[A] {
  import Option._

  /** Catamorphism.
   * Run the given function on the underlying value if present, otherwise return
   * the provided fallback value */
  final def cata[B](f: A => B, b: => B): B =
    this match {
      case Some(a) => f(a)
      case None() => b
    }

  /** Return the underlying value if present, otherwise the provided fallback value */
  final def getOrElse(a: => A): A =
    cata(identity, a)

  /** alias for [[getOrElse]] */
  final def |(a: => A): A =
    getOrElse(a)

  /** Turn the underlying value into a failure validation if present, otherwise
   * return a success validation with the provided fallback value */
  final def toInvalid[B](b: => B): Validated[A, B] =
    cata(Validated.Invalid(_), Validated.Valid(b))

  /** Turn the underlying value into a success validation if present, otherwise
   * return a failure validation with the provided fallback value */
  final def toValid[B](b: => B): Validated[B, A] =
    cata(Validated.Valid(_), Validated.Invalid(b))

  /** Turn the underlying value into a left disjunction if present, otherwise
   * return a right disjunction with the provided fallback value */
  final def toLeft[B](b: => B): A Xor B =
    cata(Xor.left(_), Xor.right(b))

  /** alias for [[toLeft]] */
  final def <\/[B](b: => B): A Xor B =
    toLeft(b)

  /** Turn the underlying value into a right disjunction if present, otherwise
   * return a left disjunction with the provided fallback value */
  final def toRight[B](b: => B): B Xor A =
    cata(Xor.right(_), Xor.left(b))

  /** alias for [[toRight]] */
  final def \/>[B](b: => B): B Xor A =
    toRight(b)

  /** True if an underlying value is present */
  final def isSome: Boolean =
    cata(_ => true, false)

  /** True if no underlying value is present */
  final def isNone: Boolean =
    cata(_ => false, true)

  /**
   * Return a new Option which is the result of applying a function to
   * the value if a value is present.
   */
  final def map[B](f: A => B): Option[B] =
    cata(f andThen some[B], none[B])

  /**
   * Return a new Option which is the result of applying a function to
   * the value if a value is present.
   */
  final def flatMap[B](f: A => Option[B]): Option[B] =
    cata(f, none[B])

  final def foldLeft[B](b: B)(f: (B,A) => B): B = this match {
    case Some(a) => f(b,a)
    case _ => b
  }

  final def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    this match {
      case Some(a) => f(a,b)
      case _ => b
    }

  /**
   * Call the side-effecting function with the value if one is present.
   */
  final def foreach(f: A => Unit): Unit =
    this match {
      case Some(a) => f(a)
      case _ => ()
    }

  /** Convert to a standard library `Option` */
  final def toStdOption: scala.Option[A] =
    cata(scala.Some(_), scala.None)

  /** Return this instance if it is a [[Some]], otherwise the provided fallback */
  final def orElse(oa: => Option[A]): Option[A] =
    cata(_ => this, oa)

  final def coflatten: Option[Option[A]] = map(some)

  final def coflatMap[B](f: Option[A] => B): Option[B] =
    map(_ => f(this))

  final def zip[B](fb: Option[B]): Option[(A, B)] =
    for {
      a <- this
      b <- fb
    } yield (a, b)

  final def zipWith[B, C](fb: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- this
      b <- fb
    } yield f(a, b)

  final def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) this else none)

  final def filterNot(f: A => Boolean): Option[A] =
    filter(f.andThen(!_))

  /** Return `true` if this is a [[None]] or if this is a [[Some]]
   * and the underlying value satisfies the provided predicate */
  final def forall(f: A => Boolean): Boolean =
    cata(f, true)

  /** Return `true` if this is a [[Some]] and the underlying value
   * satisfies the provided predicate */
  final def exists(f: A => Boolean): Boolean =
    cata(f, false)

  /** Return `true` if this is a [[Some]] and the underlying value equals the provided value
   * otherwise return `false` */
  final def contains(a: A): Boolean =
    cata(_ == a, false)

  final def isDefined: Boolean = this != None()
  final def isEmpty: Boolean = this == None()

  final def collect[B](f: PartialFunction[A,B]): Option[B] = this match {
    case Some(a) if f.isDefinedAt(a) => Some(f(a))
    case _ => None()

  }
  final def toScalaOption: scala.Option[A] = cata(scala.Some.apply,scala.None)

/*
  /** Return the underlying value if present, otherwise the monoid zero */
  final def orZero(implicit F: Monoid[A]): A =
    getOrElse(F.empty)

  /** alias for [[orZero]] */
  final def unary_~(implicit z: Monoid[A]): A =
    orZero
 */
/*
  /**
   * Return the underlying value wrapped in type `F` if present, otherwise the
   * none value for type `F` */
  final def orNone[F[_]](implicit F: Applicative[F], G: MonoidK[F]): F[A] =
    cata(F.pure(_), G.empty)
 */
}

case object None extends Option[Nothing] {
  def unapply[A](l: Option[A]) = l == None
  def apply[A]() = this.asInstanceOf[Option[A]] // YOLO
}

final case class Some[A](a: A) extends Option[A]

object Option extends OptionFunctions with OptionInstances

sealed trait OptionFunctions {

  final def apply[A](a: A): Option[A] = if(a == null) none else some(a)

  final def none[A]: Option[A] = None()
  final def some[A](a: A): Option[A] = Some(a)

  final def fromScalaOption[A](oa: scala.Option[A]): Option[A] =
    oa.fold(none[A])(some)

  def fromTryCatchNonFatal[T](a: => T): Option[T] = try {
    some(a)
  } catch {
    case NonFatal(t) => none
  }

}


trait OptionInstances extends OptionInstances1 {
  implicit val optionInstance: Traverse[Option] with MonadCombine[Option] with CoflatMap[Option] with Alternative[Option] =
    new Traverse[Option] with MonadCombine[Option] with CoflatMap[Option] with Alternative[Option] {

      override def empty[A]: Option[A] = None()

      override def combineK[A](x: Option[A], y: Option[A]): Option[A] = x orElse y

      override def pure[A](x: A): Option[A] = Some(x)

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
        fa map f

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa flatMap f

      override def map2[A, B, Z](fa: Option[A], fb: Option[B])(f: (A, B) => Z): Option[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      override def tailRecM[A, B](a: A)(f: A => Option[scala.Either[A, B]]): Option[B] = {
        @tailrec
        def go(o: Option[scala.Either[A,B]]): Option[B] = o match {
          case None() => None()
          case Some(scala.Left(a)) => go(f(a))
          case Some(scala.Right(b)) => Some(b)
        }

        go(f(a))
      }

      override def coflatMap[A, B](fa: Option[A])(f: Option[A] => B): Option[B] = fa coflatMap f

      def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)

      override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
        fa match {
          case None() => Applicative[G].pure(None())
          case Some(a) => Applicative[G].map(f(a))(Some(_))
        }

      override def exists[A](fa: Option[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Option[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Option[A]): Boolean =
        fa.isEmpty
    }

  implicit def optionMonoid[A](implicit ev: Semigroup[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def empty: Option[A] = None()
      def combine(x: Option[A], y: Option[A]): Option[A] =
        x match {
          case None() => y
          case Some(xx) => y match {
            case None() => x
            case Some(yy) => Some(ev.combine(xx,yy))
          }
        }
    }

  implicit def orderOption[A](implicit ev: Order[A]): Order[Option[A]] =
    new Order[Option[A]] {
      def compare(x: Option[A], y: Option[A]): Int =
        x match {
          case Some(a) =>
            y match {
              case Some(b) => ev.compare(a, b)
              case None => 1
            }
          case None =>
            if (y.isDefined) -1 else 0
        }
    }

  implicit def showOption[A](implicit A: Show[A]): Show[Option[A]] =
    new Show[Option[A]] {
      def show(fa: Option[A]): String = fa match {
        case Some(a) => s"Some(${A.show(a)})"
        case None => "None"
      }
    }
}

private[dogs] sealed trait OptionInstances1 extends OptionInstances2 {
  implicit def partialOrderOption[A](implicit ev: PartialOrder[A]): PartialOrder[Option[A]] =
    new PartialOrder[Option[A]] {
      def partialCompare(x: Option[A], y: Option[A]): Double =
        x.cata(a => y.cata(ev.partialCompare(_, a), 1.0), if (y.isDefined) -1.0 else 0.0)
    }
}

private[dogs] sealed trait OptionInstances2 {
  implicit def eqOption[A](implicit ev: Eq[A]): Eq[Option[A]] =
    new Eq[Option[A]] {
      def eqv(x: Option[A], y: Option[A]): Boolean =
        x.cata(a => y.cata(ev.eqv(_, a), false), y == None)
    }
}


