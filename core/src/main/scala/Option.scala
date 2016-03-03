package dogs

import Predef._
import scala.util.control.NonFatal
import cats.data.{Validated,ValidatedNel,Xor}

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

object Option extends OptionFunctions {
}

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

